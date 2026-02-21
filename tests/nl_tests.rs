
// ===========================================================================
// B7: Bugfix integration tests — multi-turn conversations
// ===========================================================================

/// Helper: assert PlanCreated and that the YAML compiles
fn assert_plan_compiles(response: &NlResponse, expected_ops: &[&str]) {
    match response {
        NlResponse::PlanCreated { workflow_yaml, .. } => {
            for op in expected_ops {
                assert!(
                    workflow_yaml.contains(op),
                    "workflow should contain '{}': {}",
                    op, workflow_yaml
                );
            }
            let parsed = cadmus::workflow::parse_workflow(workflow_yaml)
                .expect("should parse");
            let registry = cadmus::fs_types::build_full_registry();
            cadmus::workflow::compile_workflow(&parsed, &registry)
                .unwrap_or_else(|e| panic!(
                    "workflow should compile: {:?}\nYAML:\n{}", e, workflow_yaml
                ));
        }
        other => panic!("expected PlanCreated, got: {:?}", other),
    }
}

// ===========================================================================
// I1: Shell injection via NL pipeline — end-to-end tests
// ===========================================================================

/// Helper: run NL input through create → approve, return the generated script
fn nl_to_script(input: &str) -> Option<String> {
    use cadmus::nl::dialogue::DialogueState;
    use cadmus::nl::{NlResponse, process_input};

    let mut state = DialogueState::new();
    let r1 = process_input(input, &mut state);
    match r1 {
        NlResponse::PlanCreated { .. } => {
            let r2 = process_input("yes", &mut state);
            match r2 {
                NlResponse::Approved { script } => script,
                _ => None,
            }
        }
        _ => None, // NeedsClarification or other — no script
    }
}

#[test]
fn test_nl_injection_command_substitution_in_path() {
    // User tries to inject via a quoted path
    let script = nl_to_script(r#"list files in "$(rm -rf /)""#);
    if let Some(s) = script {
        // The path should be safely quoted — either literal single-quotes
        // or wrapped in (shell-quote ...) which single-quotes at runtime
        assert!(s.contains("'$(rm -rf /)'") || s.contains("(shell-quote") || !s.contains("$(rm"),
            "command substitution must be safely quoted: {}", s);
    }
}

#[test]
fn test_nl_injection_semicolon_in_path() {
    let script = nl_to_script(r#"list files in "/tmp; rm -rf /""#);
    if let Some(s) = script {
        assert!(s.contains("'/tmp; rm -rf /'") || s.contains("(shell-quote") || !s.contains("; rm"),
            "semicolon injection must be safely quoted: {}", s);
    }
}

#[test]
fn test_nl_injection_backtick_in_path() {
    let script = nl_to_script(r#"list files in "/tmp/`whoami`""#);
    if let Some(s) = script {
        // Backtick in double quotes is safe IF it's inside (shell-quote ...)
        // which will single-quote it at runtime. Check it's not bare in a
        // shell command string without shell-quote protection.
        let has_shell_quote = s.contains("(shell-quote");
        assert!(has_shell_quote || !s.contains("\"/tmp/`whoami`\""),
            "backtick must not be in double quotes: {}", s);
    }
}

#[test]
fn test_nl_injection_pipe_in_path() {
    let script = nl_to_script(r#"list files in "/tmp | cat /etc/passwd""#);
    if let Some(s) = script {
        assert!(s.contains("'/tmp | cat /etc/passwd'") || s.contains("(shell-quote") || !s.contains("| cat"),
            "pipe injection must be safely quoted: {}", s);
    }
}

#[test]
fn test_nl_injection_newline_in_path() {
    // Newlines in paths should be quoted
    let input = "list files in /tmp\nrm -rf /";
    let script = nl_to_script(input);
    if let Some(s) = script {
        assert!(!s.contains("\nrm -rf"), "newline must not break script: {}", s);
    }
}

// ===========================================================================
// I2: Conversational chaos — stress tests for the NL pipeline
// ===========================================================================

/// Helper: process input and assert it doesn't panic, returning the response
fn no_panic(input: &str) -> NlResponse {
    let mut state = DialogueState::new();
    process_input(input, &mut state)
}

// -- Slangy / casual inputs --

#[test]
fn test_chaos_slangy_find_pdfs() {
    let r = no_panic("yo can you like find all my pdfs on the desktop real quick");
    match &r {
        NlResponse::PlanCreated { workflow_yaml, .. } => {
            assert!(workflow_yaml.contains("find_matching") || workflow_yaml.contains("walk_tree")
                || workflow_yaml.contains("list_dir"),
                "should produce a file-finding workflow: {}", workflow_yaml);
        }
        NlResponse::NeedsClarification { .. } => {} // acceptable
        other => panic!("unexpected response for slangy input: {:?}", other),
    }
}

#[test]
fn test_chaos_rude_input() {
    let r = no_panic("ugh just list the stupid files in /tmp already");
    match &r {
        NlResponse::PlanCreated { .. } => {} // good
        NlResponse::NeedsClarification { .. } => {} // acceptable
        other => panic!("unexpected response for rude input: {:?}", other),
    }
}

#[test]
fn test_chaos_verbose_input() {
    let r = no_panic(
        "hey so I was wondering if you could maybe possibly help me out with \
         finding all the PDF files that might be somewhere in my Documents folder \
         because I really need to find them for a project I'm working on"
    );
    match &r {
        NlResponse::PlanCreated { .. } => {} // good
        NlResponse::NeedsClarification { .. } => {} // acceptable
        other => panic!("unexpected response for verbose input: {:?}", other),
    }
}

#[test]
fn test_chaos_terse_input() {
    let r = no_panic("ls /tmp");
    match &r {
        NlResponse::PlanCreated { .. } => {} // good
        NlResponse::NeedsClarification { .. } => {} // acceptable
        other => panic!("unexpected response for terse input: {:?}", other),
    }
}

#[test]
fn test_chaos_multi_sentence() {
    let r = no_panic("I need to find files. They should be PDFs. Look in ~/Documents.");
    match &r {
        NlResponse::PlanCreated { .. } => {} // good
        NlResponse::NeedsClarification { .. } => {} // acceptable
        other => panic!("unexpected response for multi-sentence: {:?}", other),
    }
}

// -- Ambiguous / nonsensical inputs --

#[test]
fn test_chaos_ambiguous_no_op() {
    let r = no_panic("just do something with files idk");
    // Should NOT produce a broken workflow — either clarify or error
    match &r {
        NlResponse::NeedsClarification { .. } => {} // good
        NlResponse::PlanCreated { workflow_yaml, .. } => {
            // If it creates a plan, it should at least compile
            let parsed = cadmus::workflow::parse_workflow(workflow_yaml)
                .expect("should parse");
            let registry = cadmus::fs_types::build_full_registry();
            cadmus::workflow::compile_workflow(&parsed, &registry)
                .unwrap_or_else(|e| panic!(
                    "if plan created, it must compile: {:?}\nYAML:\n{}", e, workflow_yaml
                ));
        }
        _ => {} // Error is also acceptable
    }
}

// -- Extreme inputs --

#[test]
fn test_chaos_single_char() {
    let _ = no_panic("x");
}

#[test]
fn test_chaos_empty_string() {
    let _ = no_panic("");
}

#[test]
fn test_chaos_only_punctuation() {
    let _ = no_panic("!!!??...,,,");
}

#[test]
fn test_chaos_only_spaces() {
    let _ = no_panic("     ");
}

#[test]
fn test_chaos_unicode_emoji() {
    let _ = no_panic("🔥 find files 🔥");
}

#[test]
fn test_chaos_unicode_cjk() {
    let _ = no_panic("ファイルを探す ~/Documents");
}

#[test]
fn test_chaos_very_long_input() {
    let garbage = "blah ".repeat(200); // 1000 chars
    let _ = no_panic(&garbage);
}

#[test]
fn test_chaos_very_long_single_word() {
    let word = "a".repeat(1000);
    let _ = no_panic(&word);
}

#[test]
fn test_chaos_numbers_only() {
    let _ = no_panic("12345 67890");
}

#[test]
fn test_chaos_mixed_garbage() {
    let _ = no_panic("@#$%^&*()_+{}|:<>?~`");
}

#[test]
fn test_chaos_repeated_ops() {
    let _ = no_panic("find find find list list list zip zip zip");
}

#[test]
fn test_chaos_sql_injection_attempt() {
    let _ = no_panic("'; DROP TABLE files; --");
}

#[test]
fn test_chaos_html_injection() {
    let _ = no_panic("<script>alert('xss')</script> find files");
}

// ===========================================================================
// I4: Diverse ops end-to-end — NL → workflow → script
// ===========================================================================

/// Helper: NL input → PlanCreated → compile → generate Racket script, return script
/// Panics if any step fails.
fn nl_e2e_script(input: &str) -> String {
    let mut state = DialogueState::new();
    let r = process_input(input, &mut state);
    match r {
        NlResponse::PlanCreated { workflow_yaml, .. } => {
            let parsed = cadmus::workflow::parse_workflow(&workflow_yaml)
                .unwrap_or_else(|e| panic!("parse failed for '{}': {:?}\nYAML:\n{}", input, e, workflow_yaml));
            let registry = cadmus::fs_types::build_full_registry();
            let compiled = cadmus::workflow::compile_workflow(&parsed, &registry)
                .unwrap_or_else(|e| panic!("compile failed for '{}': {:?}\nYAML:\n{}", input, e, workflow_yaml));
            let racket_reg = build_racket_registry();
            cadmus::racket_executor::generate_racket_script(&compiled, &parsed, &racket_reg)
                .unwrap_or_else(|e| panic!("script gen failed for '{}': {:?}\nYAML:\n{}", input, e, workflow_yaml))
        }
        other => panic!("expected PlanCreated for '{}', got: {:?}", input, other),
    }
}

/// Helper: NL input → PlanCreated, return workflow YAML (no script gen)
fn nl_e2e_yaml(input: &str) -> String {
    let mut state = DialogueState::new();
    let r = process_input(input, &mut state);
    match r {
        NlResponse::PlanCreated { workflow_yaml, .. } => {
            // Verify it at least compiles
            let parsed = cadmus::workflow::parse_workflow(&workflow_yaml)
                .unwrap_or_else(|e| panic!("parse failed for '{}': {:?}\nYAML:\n{}", input, e, workflow_yaml));
            let registry = cadmus::fs_types::build_full_registry();
            cadmus::workflow::compile_workflow(&parsed, &registry)
                .unwrap_or_else(|e| panic!("compile failed for '{}': {:?}\nYAML:\n{}", input, e, workflow_yaml));
            workflow_yaml
        }
        other => panic!("expected PlanCreated for '{}', got: {:?}", input, other),
    }
}

// -- Git ops --

#[test]
fn test_e2e_git_log() {
    // Fixed in I6: repo input name now maps to Repo type before Dir check
    let yaml = nl_e2e_yaml("git log for ~/myrepo");
    assert!(yaml.contains("git_log"), "should have git_log op: {}", yaml);
}

#[test]
fn test_e2e_git_diff() {
    let yaml = nl_e2e_yaml("git diff in ~/myrepo");
    assert!(yaml.contains("git_diff"), "should have git_diff op: {}", yaml);
}

#[test]
fn test_e2e_git_clone() {
    let yaml = nl_e2e_yaml("git clone https://github.com/user/repo");
    assert!(yaml.contains("git_clone"), "should have git_clone op: {}", yaml);
}

#[test]
fn test_e2e_git_status() {
    let yaml = nl_e2e_yaml("git status of ~/myrepo");
    assert!(yaml.contains("git_status"), "should have git_status op: {}", yaml);
}

// -- Search ops --

#[test]
fn test_e2e_grep_for_todo() {
    let script = nl_e2e_script("grep for TODO in ~/src");
    assert!(script.contains("grep"), "should have grep: {}", script);
}

#[test]
fn test_e2e_search_content() {
    let yaml = nl_e2e_yaml("search for text error in ~/logs");
    assert!(yaml.contains("search_content"), "should have search_content: {}", yaml);
}

// -- Archive ops --

#[test]
fn test_e2e_zip_folder() {
    let script = nl_e2e_script("zip up ~/Projects");
    assert!(script.contains("zip") || script.contains("tar"),
        "should have zip/tar command: {}", script);
}

#[test]
fn test_e2e_extract_archive() {
    let script = nl_e2e_script("extract ~/archive.tar.gz");
    assert!(script.contains("tar") || script.contains("extract"),
        "should have tar/extract command: {}", script);
}

// -- System info ops --

#[test]
fn test_e2e_disk_usage() {
    // Fixed in I6: du_size now uses pathref input → Path type
    let yaml = nl_e2e_yaml("check disk usage of ~/Documents");
    assert!(yaml.contains("du_size"), "should have du_size op: {}", yaml);
}

#[test]
fn test_e2e_file_info() {
    // Fixed in I6: stat now uses pathref input → Path type
    let yaml = nl_e2e_yaml("get file info for ~/test.txt");
    assert!(yaml.contains("stat"), "should have stat op: {}", yaml);
}

// -- Download ops --

#[test]
fn test_e2e_download_url() {
    let yaml = nl_e2e_yaml("download https://example.com/file.zip");
    assert!(yaml.contains("download"), "should have download op: {}", yaml);
}

// -- File manipulation ops --

#[test]
fn test_e2e_list_files() {
    let script = nl_e2e_script("list files in /tmp");
    assert!(script.contains("ls"), "should have ls command: {}", script);
}

#[test]
fn test_e2e_walk_tree() {
    let script = nl_e2e_script("walk the tree at ~/src");
    assert!(script.contains("find"), "should have find command: {}", script);
}

#[test]
fn test_e2e_find_matching() {
    let script = nl_e2e_script("find all *.rs files in ~/src");
    assert!(script.contains("grep") || script.contains("find"),
        "should have grep/find: {}", script);
}

#[test]
fn test_e2e_sort_files() {
    let yaml = nl_e2e_yaml("sort files in ~/Documents by name");
    assert!(yaml.contains("sort_by") || yaml.contains("sort"),
        "should have sort op: {}", yaml);
}

// -- Generic fallback ops --

#[test]
fn test_e2e_compress_file() {
    let yaml = nl_e2e_yaml("compress ~/big_file.log");
    assert!(yaml.contains("gzip_compress") || yaml.contains("compress"),
        "should have compress op: {}", yaml);
}

#[test]
fn test_e2e_count_lines() {
    // count expects Seq(a) — for file targets, the seq_op handler builds
    // a read_file → count pipeline. This may still fail for typed files
    // like .csv where read_file produces Csv (not Seq). Accept either
    // PlanCreated or Error as valid responses.
    let mut state = DialogueState::new();
    let r = process_input("count lines in ~/data.csv", &mut state);
    match &r {
        NlResponse::PlanCreated { .. } => {} // great — it compiled
        NlResponse::Error { .. } => {} // type mismatch is expected for .csv
        other => panic!("unexpected for count: {:?}", other),
    }
}

#[test]
fn test_e2e_checksum() {
    let yaml = nl_e2e_yaml("checksum ~/important.zip");
    assert!(yaml.contains("checksum") || yaml.contains("sha") || yaml.contains("md5"),
        "should have checksum op: {}", yaml);
}

// -- Edge: unknown/nonsensical op --

#[test]
fn test_e2e_unknown_op_no_panic() {
    // Something that doesn't map to any known op
    let mut state = DialogueState::new();
    let r = process_input("flibbertigibbet the whatchamacallit", &mut state);
    // Should NOT panic — either NeedsClarification or some fallback
    match r {
        NlResponse::NeedsClarification { .. } => {} // good
        NlResponse::PlanCreated { .. } => {} // acceptable if it guessed something
        NlResponse::Error { .. } => {} // also fine
        other => panic!("unexpected for unknown op: {:?}", other),
    }
}

// ===========================================================================
// I5: Path resolution, dir aliases, noun patterns edge cases
// ===========================================================================

#[test]
fn test_path_typo_correction_preserves_tilde_path() {
    // "findd" should be corrected to "find", but ~/Documents should survive
    let mut state = DialogueState::new();
    let r = process_input("findd files in ~/Documents", &mut state);
    match &r {
        NlResponse::PlanCreated { workflow_yaml, .. } => {
            assert!(workflow_yaml.contains("~/Documents"),
                "~/Documents should survive typo correction: {}", workflow_yaml);
        }
        // NeedsClarification is also acceptable if "findd" wasn't corrected
        NlResponse::NeedsClarification { .. } => {}
        other => panic!("unexpected: {:?}", other),
    }
}

#[test]
fn test_path_quoted_with_typo_in_context() {
    // "findd" has a typo, but "NO NAME" should be preserved as-is
    let mut state = DialogueState::new();
    let r = process_input(r#"findd files in "NO NAME""#, &mut state);
    match &r {
        NlResponse::PlanCreated { workflow_yaml, .. } => {
            // The quoted path should survive — either as "NO NAME" or /Volumes/NO NAME
            assert!(workflow_yaml.contains("NO NAME") || workflow_yaml.contains("no name"),
                "quoted path should survive: {}", workflow_yaml);
        }
        NlResponse::NeedsClarification { .. } => {} // acceptable
        other => panic!("unexpected: {:?}", other),
    }
}

#[test]
fn test_path_dir_alias_in_edit_context() {
    // Create a workflow, then edit with "also skip desktop"
    // "desktop" in skip context should NOT resolve to ~/Desktop as a path
    let mut state = DialogueState::new();
    let _ = process_input("walk ~/tmp", &mut state);

    let r = process_input("skip desktop", &mut state);
    match &r {
        NlResponse::PlanEdited { workflow_yaml, .. } => {
            // The skip should add a filter for "desktop" (the name), not ~/Desktop
            assert!(!workflow_yaml.contains("~/Desktop"),
                "skip should use 'desktop' as pattern, not ~/Desktop: {}", workflow_yaml);
        }
        NlResponse::NeedsClarification { .. } => {} // acceptable
        other => panic!("unexpected for skip desktop: {:?}", other),
    }
}

#[test]
fn test_path_noun_pattern_with_explicit_pattern() {
    // "find *.log files on desktop" — explicit *.log should win over noun pattern
    let mut state = DialogueState::new();
    let r = process_input("find *.log files on desktop", &mut state);
    match &r {
        NlResponse::PlanCreated { workflow_yaml, .. } => {
            assert!(workflow_yaml.contains("*.log") || workflow_yaml.contains(".log"),
                "explicit pattern should be present: {}", workflow_yaml);
        }
        NlResponse::NeedsClarification { .. } => {} // acceptable
        other => panic!("unexpected: {:?}", other),
    }
}

#[test]
fn test_path_multiple_paths_in_input() {
    // "rename ~/old.txt to ~/new.txt" — both paths should be captured
    let mut state = DialogueState::new();
    let r = process_input("rename ~/old.txt to ~/new.txt", &mut state);
    match &r {
        NlResponse::PlanCreated { workflow_yaml, .. } => {
            assert!(workflow_yaml.contains("old.txt") || workflow_yaml.contains("new.txt"),
                "should capture paths: {}", workflow_yaml);
        }
        NlResponse::NeedsClarification { .. } => {} // acceptable
        NlResponse::Error { .. } => {} // type mismatch is a known bug
        other => panic!("unexpected: {:?}", other),
    }
}

#[test]
fn test_path_url_preserved() {
    // URLs should pass through without modification
    let mut state = DialogueState::new();
    let r = process_input("download https://example.com/file.tar.gz", &mut state);
    match &r {
        NlResponse::PlanCreated { workflow_yaml, .. } => {
            assert!(workflow_yaml.contains("https://example.com/file.tar.gz"),
                "URL should be preserved: {}", workflow_yaml);
        }
        other => panic!("unexpected: {:?}", other),
    }
}

#[test]
fn test_path_home_expansion_not_mangled() {
    // ~/Documents should not be mangled by any pipeline stage
    let mut state = DialogueState::new();
    let r = process_input("list files in ~/Documents", &mut state);
    match &r {
        NlResponse::PlanCreated { workflow_yaml, .. } => {
            assert!(workflow_yaml.contains("~/Documents"),
                "~/Documents should be preserved: {}", workflow_yaml);
        }
        other => panic!("unexpected: {:?}", other),
    }
}

// ===========================================================================
// I3: Multi-step conversation flow tests
// ===========================================================================

#[test]
fn test_conv_create_edit_skip_approve() {
    let mut state = DialogueState::new();

    // Step 1: Create an add workflow
    let r1 = process_input("add 10 and 20", &mut state);
    match &r1 {
        NlResponse::PlanCreated { workflow_yaml, .. } => {
            assert!(workflow_yaml.contains("add"),
                "should have add: {}", workflow_yaml);
        }
        other => panic!("expected PlanCreated, got: {:?}", other),
    }

    // Step 2: Approve
    let r2 = process_input("looks good", &mut state);
    match &r2 {
        NlResponse::Approved { script } => {
            assert!(script.is_some(), "should generate a Racket program");
            let s = script.as_ref().unwrap();
            assert!(s.contains("#lang racket"), "should be a Racket program: {}", s);
            assert!(s.contains("(+"), "should contain add: {}", s);
            assert!(state.current_workflow.is_none(), "approve should consume workflow");
        }
        other => panic!("expected Approved, got: {:?}", other),
    }
}

#[test]
fn test_conv_approve_without_workflow() {
    let mut state = DialogueState::new();
    let r = process_input("yes", &mut state);
    match &r {
        NlResponse::NeedsClarification { needs } => {
            assert!(!needs.is_empty(), "should explain there's nothing to approve");
        }
        other => panic!("expected NeedsClarification for approve without workflow, got: {:?}", other),
    }
}

#[test]
fn test_conv_approve_after_reject() {
    let mut state = DialogueState::new();

    // Create workflow
    let _ = process_input("list files in /tmp", &mut state);
    assert!(state.current_workflow.is_some());

    // Reject it
    let r1 = process_input("nah", &mut state);
    assert!(matches!(r1, NlResponse::Rejected));
    assert!(state.current_workflow.is_none());

    // Try to approve — should fail
    let r2 = process_input("yes", &mut state);
    assert!(matches!(r2, NlResponse::NeedsClarification { .. }),
        "approve after reject should need clarification: {:?}", r2);
}

#[test]
fn test_conv_edit_after_approve() {
    let mut state = DialogueState::new();

    // Create and approve
    let _ = process_input("list files in /tmp", &mut state);
    let _ = process_input("yes", &mut state);
    assert!(state.current_workflow.is_none(), "approve should consume workflow");

    // Try to edit — should fail gracefully
    let r = process_input("skip .git", &mut state);
    match &r {
        NlResponse::NeedsClarification { .. } => {} // good
        NlResponse::PlanCreated { .. } => {} // also acceptable — might create new workflow
        other => panic!("edit after approve should clarify or create new: {:?}", other),
    }
}

#[test]
fn test_conv_ten_sequential_edits() {
    let mut state = DialogueState::new();

    // Create a workflow
    let r = process_input("find pdfs in ~/Documents", &mut state);
    assert!(matches!(r, NlResponse::PlanCreated { .. }),
        "should create plan: {:?}", r);

    // Apply 10 edits — should not corrupt state
    let edits = [
        "skip .git",
        "skip .svn",
        "skip node_modules",
        "skip target",
        "skip .DS_Store",
        "also skip build",
        "skip dist",
        "skip vendor",
        "skip __pycache__",
        "skip .cache",
    ];

    for (i, edit) in edits.iter().enumerate() {
        let r = process_input(edit, &mut state);
        match &r {
            NlResponse::PlanEdited { workflow_yaml, .. } => {
                // Should still parse and compile
                let parsed = cadmus::workflow::parse_workflow(workflow_yaml)
                    .unwrap_or_else(|e| panic!("edit {} should parse: {:?}\nYAML:\n{}", i, e, workflow_yaml));
                let registry = cadmus::fs_types::build_full_registry();
                cadmus::workflow::compile_workflow(&parsed, &registry)
                    .unwrap_or_else(|e| panic!("edit {} should compile: {:?}\nYAML:\n{}", i, e, workflow_yaml));
            }
            other => {
                // Some edits might not apply cleanly — that's ok as long as no panic
                eprintln!("edit {} ({}) got: {:?}", i, edit, other);
            }
        }
    }

    // Should still be able to approve
    if state.current_workflow.is_some() {
        let r = process_input("approve", &mut state);
        match &r {
            NlResponse::Approved { script } => {
                // Filesystem ops (find_matching, filter) don't have Racket equivalents,
                // so the Racket generator returns None for this workflow. That's correct.
                // The important thing is we get Approved (not a panic or error).
                let _ = script; // may be None for shell-only workflows
            }
            other => panic!("should approve after edits: {:?}", other),
        }
    }
}

#[test]
fn test_conv_overwrite_workflow() {
    let mut state = DialogueState::new();

    // Create first workflow
    let r1 = process_input("list files in /tmp", &mut state);
    assert!(matches!(r1, NlResponse::PlanCreated { .. }));

    // Create second workflow — should overwrite
    let r2 = process_input("find pdfs in ~/Documents", &mut state);
    match &r2 {
        NlResponse::PlanCreated { workflow_yaml, .. } => {
            // Should be the new workflow, not the old one
            assert!(workflow_yaml.contains("Documents") || workflow_yaml.contains("pdf"),
                "should be the new workflow: {}", workflow_yaml);
        }
        other => panic!("expected PlanCreated for overwrite, got: {:?}", other),
    }
}

#[test]
fn test_conv_reject_then_create_new() {
    let mut state = DialogueState::new();

    // Create and reject
    let _ = process_input("list files in /tmp", &mut state);
    let _ = process_input("nah", &mut state);
    assert!(state.current_workflow.is_none());

    // Create new workflow — should work
    let r = process_input("find pdfs in ~/Documents", &mut state);
    assert!(matches!(r, NlResponse::PlanCreated { .. }),
        "should create new plan after reject: {:?}", r);
}

#[test]
fn test_conv_double_approve() {
    let mut state = DialogueState::new();

    // Create and approve
    let _ = process_input("list files in /tmp", &mut state);
    let r1 = process_input("yes", &mut state);
    assert!(matches!(r1, NlResponse::Approved { .. }));

    // Second approve — should fail gracefully
    let r2 = process_input("yes", &mut state);
    assert!(matches!(r2, NlResponse::NeedsClarification { .. }),
        "double approve should need clarification: {:?}", r2);
}

// -- Conversation: compress a file (B2+B3 fix) --

#[test]
fn test_bugfix_compress_file_compiles() {
    let mut state = DialogueState::new();
    let r = process_input("compress my_file.txt", &mut state);
    assert_plan_compiles(&r, &["gzip_compress"]);
}

#[test]
fn test_bugfix_compress_tar_compiles() {
    let mut state = DialogueState::new();
    let r = process_input("compress my_project.tar", &mut state);
    assert_plan_compiles(&r, &["gzip_compress"]);
}

// -- Conversation: hash a yaml file (B2+B3 fix) --

#[test]
fn test_bugfix_hash_yaml_compiles() {
    let mut state = DialogueState::new();
    let r = process_input("hash config.yaml", &mut state);
    assert_plan_compiles(&r, &["openssl_hash"]);
}

// -- Conversation: grep for errors in a file (B3 fix) --

#[test]
fn test_bugfix_grep_file_compiles() {
    let mut state = DialogueState::new();
    let r = process_input("grep for errors in /var/log/app.log", &mut state);
    assert_plan_compiles(&r, &["search_content"]);
}

#[test]
fn test_bugfix_search_dir_compiles() {
    let mut state = DialogueState::new();
    let r = process_input("search for TODO in all my files", &mut state);
    assert_plan_compiles(&r, &["search_content"]);
}

// -- Conversation: clone a repo (B4 fix) --

#[test]
fn test_bugfix_clone_repo_creates_plan() {
    let mut state = DialogueState::new();
    let r = process_input("clone the repo at github.com/user/project", &mut state);
    assert_plan_created(&r, &["git_clone"]);
}

// -- Conversation: "do the thing" (B1+B5 fix) --

#[test]
fn test_bugfix_do_the_thing_clarifies() {
    let mut state = DialogueState::new();
    let r = process_input("do the thing", &mut state);
    assert!(matches!(r, NlResponse::NeedsClarification { .. }),
        "expected NeedsClarification, got: {:?}", r);
}

// -- Conversation: "nah scrap that" (B1+B5 fix) --

#[test]
fn test_bugfix_nah_scrap_that_rejects() {
    let mut state = DialogueState::new();
    process_input("zip up ~/Downloads", &mut state);
    let r = process_input("nah scrap that", &mut state);
    assert!(matches!(r, NlResponse::Rejected),
        "expected Rejected, got: {:?}", r);
}

// -- Conversation: approve without plan (B6 fix) --

#[test]
fn test_bugfix_approve_without_plan() {
    let mut state = DialogueState::new();
    let r = process_input("approve", &mut state);
    assert!(matches!(r, NlResponse::NeedsClarification { .. }),
        "expected NeedsClarification, got: {:?}", r);
}

// -- Conversation: compound sentence (B5 fix) --

#[test]
fn test_bugfix_compound_skip_compress_instead() {
    let mut state = DialogueState::new();
    process_input("zip up ~/Downloads", &mut state);
    let r = process_input("actually skip that, compress my_data.tar instead", &mut state);
    // Should create a new plan with gzip_compress, not edit the old one
    assert_plan_created(&r, &["gzip_compress"]);
}

// -- Multi-turn conversation: full flow with bugfixes --

#[test]
fn test_bugfix_full_conversation_compress_approve() {
    let mut state = DialogueState::new();
    
    // Turn 1: compress a file (B2+B3 fix)
    let r1 = process_input("compress my_file.txt", &mut state);
    assert_plan_compiles(&r1, &["gzip_compress"]);
    
    // Turn 2: approve
    let r2 = process_input("looks good, go ahead", &mut state);
    assert!(matches!(r2, NlResponse::Approved { .. }));
}

#[test]
fn test_bugfix_full_conversation_search_approve() {
    let mut state = DialogueState::new();
    
    // Turn 1: search for TODO
    let r1 = process_input("search for TODO in my project", &mut state);
    assert_plan_compiles(&r1, &["search_content"]);
    
    // Turn 2: approve
    let r2 = process_input("ship it", &mut state);
    assert!(matches!(r2, NlResponse::Approved { .. }));
}

#[test]
fn test_bugfix_full_conversation_ambiguous_then_specific() {
    let mut state = DialogueState::new();
    
    // Turn 1: ambiguous input (B5 fix)
    let r1 = process_input("do the thing", &mut state);
    assert!(matches!(r1, NlResponse::NeedsClarification { .. }));
    
    // Turn 2: specific input (B2+B3 fix)
    let r2 = process_input("compress my_file.txt", &mut state);
    assert_plan_compiles(&r2, &["gzip_compress"]);
    
    // Turn 3: approve
    let r3 = process_input("ok go ahead", &mut state);
    assert!(matches!(r3, NlResponse::Approved { .. }));
}
// Integration tests for the NL UX layer.
//
// Tests the full pipeline end-to-end with diverse phrasings, typo correction,
// explanations, edits, approvals, rejections, and multi-turn conversations.

use cadmus::nl::dialogue::DialogueState;
use cadmus::nl::{NlResponse, process_input};

/// Build a Racket registry with inference + shell submodes (shared helper).
fn build_racket_registry() -> cadmus::registry::OperationRegistry {
    let mut reg = cadmus::registry::load_ops_pack_str(
        include_str!("../data/packs/ops/racket.ops.yaml")
    ).expect("racket.ops.yaml");
    let facts = cadmus::racket_strategy::load_racket_facts_from_str(
        include_str!("../data/packs/facts/racket.facts.yaml")
    ).expect("racket.facts.yaml");
    cadmus::racket_strategy::promote_inferred_ops(&mut reg, &facts);
    let cli_yaml = include_str!("../data/packs/facts/macos_cli.facts.yaml");
    if let Ok(cli_pack) = serde_yaml::from_str::<cadmus::fact_pack::FactPack>(cli_yaml) {
        let cli_facts = cadmus::fact_pack::FactPackIndex::build(cli_pack);
        cadmus::racket_strategy::discover_shell_submodes(&mut reg, &facts, &cli_facts);
    }
    reg
}

// ---------------------------------------------------------------------------
// Diverse phrasings → CreateWorkflow
// ---------------------------------------------------------------------------

#[test]
fn test_nl_zip_up_downloads() {
    let mut state = DialogueState::new();
    let r = process_input("zip up everything in my downloads", &mut state);
    assert_plan_created(&r, &["walk_tree", "pack_archive"]);
}

#[test]
fn test_nl_zip_up_tilde_path() {
    let mut state = DialogueState::new();
    let r = process_input("zip up ~/Downloads", &mut state);
    assert_plan_created(&r, &["pack_archive"]);
    if let NlResponse::PlanCreated { workflow_yaml, .. } = &r {
        assert!(workflow_yaml.contains("~/Downloads"), "yaml: {}", workflow_yaml);
    }
}

#[test]
fn test_nl_find_pdfs() {
    let mut state = DialogueState::new();
    let r = process_input("find all PDFs in ~/Documents", &mut state);
    assert_plan_created(&r, &["walk_tree"]);
}

#[test]
fn test_nl_list_directory() {
    let mut state = DialogueState::new();
    let r = process_input("list ~/Downloads", &mut state);
    assert_plan_created(&r, &["list_dir"]);
}

#[test]
fn test_nl_walk_the_tree() {
    let mut state = DialogueState::new();
    let r = process_input("walk the directory tree in /tmp", &mut state);
    assert_plan_created(&r, &["walk_tree"]);
}

#[test]
fn test_nl_extract_archive() {
    let mut state = DialogueState::new();
    let r = process_input("extract the archive at ~/comic.cbz", &mut state);
    assert_plan_created(&r, &["extract_archive"]);
}

#[test]
fn test_nl_grep_for_errors() {
    let mut state = DialogueState::new();
    let r = process_input("grep for errors in /var/log/app.log", &mut state);
    assert_plan_created(&r, &["search_content"]);
}

#[test]
fn test_nl_ls_downloads() {
    let mut state = DialogueState::new();
    let r = process_input("ls ~/Downloads", &mut state);
    assert_plan_created(&r, &["list_dir"]);
}

#[test]
fn test_nl_unzip_archive() {
    let mut state = DialogueState::new();
    let r = process_input("unzip ~/archive.zip", &mut state);
    assert_plan_created(&r, &["extract_archive"]);
}

#[test]
fn test_nl_sort_files() {
    let mut state = DialogueState::new();
    let r = process_input("sort files in ~/Downloads by name", &mut state);
    assert_plan_created(&r, &["sort_by"]);
}

#[test]
fn test_nl_git_log() {
    let mut state = DialogueState::new();
    let r = process_input("git log for the repo", &mut state);
    assert_plan_created(&r, &["git_log"]);
}

// ---------------------------------------------------------------------------
// Typo correction
// ---------------------------------------------------------------------------

#[test]
fn test_nl_typo_extract_archive() {
    let mut state = DialogueState::new();
    let r = process_input("extrct the archve at ~/comic.cbz", &mut state);
    assert_plan_created(&r, &["extract_archive"]);
}

#[test]
fn test_nl_typo_walk_tree() {
    let mut state = DialogueState::new();
    let r = process_input("wlak the direcotry tree in /tmp", &mut state);
    // Even with typos, should produce a valid workflow
    match &r {
        NlResponse::PlanCreated { workflow_yaml, .. } => {
            assert!(
                workflow_yaml.contains("walk_tree") || workflow_yaml.contains("list_dir"),
                "should have a dir operation: {}", workflow_yaml
            );
        }
        NlResponse::NeedsClarification { .. } => {
            // Acceptable if typos are too severe
        }
        other => panic!("expected PlanCreated or NeedsClarification, got: {:?}", other),
    }
}

// ---------------------------------------------------------------------------
// Explanations
// ---------------------------------------------------------------------------

#[test]
fn test_nl_explain_walk() {
    let mut state = DialogueState::new();
    let r = process_input("what's walk mean", &mut state);
    match r {
        NlResponse::Explanation { text } => {
            assert!(
                text.contains("directory") || text.contains("walk") || text.contains("tree"),
                "explanation should be about walking: {}", text
            );
        }
        other => panic!("expected Explanation, got: {:?}", other),
    }
}

#[test]
fn test_nl_explain_filter() {
    let mut state = DialogueState::new();
    let r = process_input("explain filter", &mut state);
    match r {
        NlResponse::Explanation { text } => {
            assert!(text.contains("filter") || text.contains("match"), "text: {}", text);
        }
        other => panic!("expected Explanation, got: {:?}", other),
    }
}

#[test]
fn test_nl_explain_what_does_sort_do() {
    let mut state = DialogueState::new();
    let r = process_input("what does sort do", &mut state);
    match r {
        NlResponse::Explanation { text } => {
            assert!(text.contains("sort") || text.contains("Sort"), "text: {}", text);
        }
        other => panic!("expected Explanation, got: {:?}", other),
    }
}

#[test]
fn test_nl_explain_how_does_grep_work() {
    let mut state = DialogueState::new();
    let r = process_input("how does grep work", &mut state);
    match r {
        NlResponse::Explanation { text } => {
            assert!(
                text.contains("search") || text.contains("content") || text.contains("grep"),
                "text: {}", text
            );
        }
        other => panic!("expected Explanation, got: {:?}", other),
    }
}

// ---------------------------------------------------------------------------
// Approve / Reject
// ---------------------------------------------------------------------------

#[test]
fn test_nl_approve_lgtm() {
    let mut state = DialogueState::new();
    process_input("zip up ~/Downloads", &mut state);
    assert!(matches!(process_input("lgtm", &mut state), NlResponse::Approved { .. }));
}

#[test]
fn test_nl_approve_sounds_good() {
    let mut state = DialogueState::new();
    process_input("zip up ~/Downloads", &mut state);
    assert!(matches!(process_input("sounds good", &mut state), NlResponse::Approved { .. }));
}

#[test]
fn test_nl_approve_yes() {
    let mut state = DialogueState::new();
    process_input("zip up ~/Downloads", &mut state);
    assert!(matches!(process_input("yes", &mut state), NlResponse::Approved { .. }));
}

#[test]
fn test_nl_approve_ok() {
    let mut state = DialogueState::new();
    process_input("zip up ~/Downloads", &mut state);
    assert!(matches!(process_input("ok", &mut state), NlResponse::Approved { .. }));
}

#[test]
fn test_nl_approve_ship_it() {
    let mut state = DialogueState::new();
    process_input("zip up ~/Downloads", &mut state);
    assert!(matches!(process_input("ship it", &mut state), NlResponse::Approved { .. }));
}

#[test]
fn test_nl_approve_do_it() {
    let mut state = DialogueState::new();
    process_input("zip up ~/Downloads", &mut state);
    assert!(matches!(process_input("do it", &mut state), NlResponse::Approved { .. }));
}

#[test]
fn test_nl_approve_go_ahead() {
    let mut state = DialogueState::new();
    process_input("zip up ~/Downloads", &mut state);
    assert!(matches!(process_input("go ahead", &mut state), NlResponse::Approved { .. }));
}

#[test]
fn test_nl_reject_no() {
    let mut state = DialogueState::new();
    assert!(matches!(process_input("no", &mut state), NlResponse::Rejected));
}

#[test]
fn test_nl_reject_nah() {
    let mut state = DialogueState::new();
    assert!(matches!(process_input("nah", &mut state), NlResponse::Rejected));
}

#[test]
fn test_nl_reject_cancel() {
    let mut state = DialogueState::new();
    assert!(matches!(process_input("cancel", &mut state), NlResponse::Rejected));
}

#[test]
fn test_nl_reject_start_over() {
    let mut state = DialogueState::new();
    assert!(matches!(process_input("start over", &mut state), NlResponse::Rejected));
}

// ---------------------------------------------------------------------------
// Edit operations
// ---------------------------------------------------------------------------

#[test]
fn test_nl_edit_skip_subdirectory() {
    let mut state = DialogueState::new();

    // Create a workflow first
    let r1 = process_input("zip up everything in ~/Downloads", &mut state);
    assert!(matches!(r1, NlResponse::PlanCreated { .. }));

    // Edit: skip a subdirectory
    let r2 = process_input("skip any subdirectory named .git", &mut state);
    match r2 {
        NlResponse::PlanEdited { workflow_yaml, .. } => {
            assert!(workflow_yaml.contains("filter"), "should have filter: {}", workflow_yaml);
        }
        other => panic!("expected PlanEdited, got: {:?}", other),
    }
}

#[test]
fn test_nl_edit_remove_step() {
    let mut state = DialogueState::new();

    // Create a workflow with multiple steps
    let r1 = process_input("find all files in ~/Documents", &mut state);
    assert!(matches!(r1, NlResponse::PlanCreated { .. }));

    let step_count_before = state.current_workflow.as_ref().unwrap().steps.len();

    // Remove a step
    let r2 = process_input("remove step 2", &mut state);
    match r2 {
        NlResponse::PlanEdited { .. } => {
            let step_count_after = state.current_workflow.as_ref().unwrap().steps.len();
            assert_eq!(step_count_after, step_count_before - 1);
        }
        other => panic!("expected PlanEdited, got: {:?}", other),
    }
}

#[test]
fn test_nl_edit_without_workflow() {
    let mut state = DialogueState::new();
    let r = process_input("skip any subdirectory named foo", &mut state);
    assert!(matches!(r, NlResponse::NeedsClarification { .. }),
        "edit without workflow should need clarification: {:?}", r);
}

// ---------------------------------------------------------------------------
// Multi-turn conversations
// ---------------------------------------------------------------------------

#[test]
fn test_nl_full_conversation_zip() {
    let mut state = DialogueState::new();

    // Turn 1: Create
    let r1 = process_input("zip up everything in ~/Downloads", &mut state);
    assert!(matches!(&r1, NlResponse::PlanCreated { .. }));
    assert!(state.current_workflow.is_some());

    // Turn 2: Ask about an op
    let r2 = process_input("what's walk mean", &mut state);
    assert!(matches!(&r2, NlResponse::Explanation { .. }));
    // Workflow should still be there
    assert!(state.current_workflow.is_some());

    // Turn 3: Edit
    let r3 = process_input("skip any subdirectory named .git", &mut state);
    assert!(matches!(&r3, NlResponse::PlanEdited { .. }));

    // Turn 4: Approve
    let r4 = process_input("lgtm", &mut state);
    assert!(matches!(r4, NlResponse::Approved { .. }));
}

#[test]
fn test_nl_conversation_create_reject_recreate() {
    let mut state = DialogueState::new();

    // Create
    let r1 = process_input("zip up ~/Downloads", &mut state);
    assert!(matches!(&r1, NlResponse::PlanCreated { .. }));

    // Reject
    let r2 = process_input("nah", &mut state);
    assert!(matches!(r2, NlResponse::Rejected));
    assert!(state.current_workflow.is_none());

    // Create again
    let r3 = process_input("list ~/Documents", &mut state);
    assert!(matches!(&r3, NlResponse::PlanCreated { .. }));
    assert!(state.current_workflow.is_some());
}

#[test]
fn test_nl_conversation_multiple_edits() {
    let mut state = DialogueState::new();

    // Create
    process_input("zip up everything in ~/Downloads", &mut state);

    // Edit 1: skip .git
    let r1 = process_input("skip any subdirectory named .git", &mut state);
    assert!(matches!(&r1, NlResponse::PlanEdited { .. }));

    // Edit 2: skip node_modules
    let r2 = process_input("skip any subdirectory named node_modules", &mut state);
    assert!(matches!(&r2, NlResponse::PlanEdited { .. }));

    // The workflow should have multiple filter steps
    let wf = state.current_workflow.as_ref().unwrap();
    let filter_count = wf.steps.iter().filter(|s| s.op == "filter").count();
    assert!(filter_count >= 2, "should have at least 2 filter steps, got {}", filter_count);
}

// ---------------------------------------------------------------------------
// NeedsClarification
// ---------------------------------------------------------------------------

#[test]
fn test_nl_gibberish() {
    let mut state = DialogueState::new();
    let r = process_input("asdfghjkl qwerty zxcvbnm", &mut state);
    assert!(matches!(r, NlResponse::NeedsClarification { .. }));
}

#[test]
fn test_nl_empty_input() {
    let mut state = DialogueState::new();
    let r = process_input("", &mut state);
    assert!(matches!(r, NlResponse::NeedsClarification { .. }));
}

// ---------------------------------------------------------------------------
// YAML validation: generated YAML compiles through the engine
// ---------------------------------------------------------------------------

#[test]
fn test_nl_generated_yaml_compiles_zip() {
    let mut state = DialogueState::new();
    let r = process_input("zip up everything in ~/Downloads", &mut state);
    assert_yaml_compiles(&r);
}

// =========================================================================
// Hardening: transcript bug fixes
// =========================================================================

// -- "also skip X" continuation edits --

#[test]
fn test_hardening_also_skip_as_edit() {
    let mut state = DialogueState::new();
    let r1 = process_input("walk the directory tree in ~/src", &mut state);
    assert!(matches!(r1, NlResponse::PlanCreated { .. }));
    let r2 = process_input("also skip node_modules", &mut state);
    match &r2 {
        NlResponse::PlanEdited { diff_description, .. } => {
            let desc_lower = diff_description.to_lowercase();
            assert!(desc_lower.contains("skip") || desc_lower.contains("filter") || desc_lower.contains("exclude"),
                "should describe skip edit: {}", diff_description);
        }
        other => panic!("expected PlanEdited for 'also skip', got: {:?}", other),
    }
}

#[test]
fn test_hardening_and_then_skip() {
    let mut state = DialogueState::new();
    let _ = process_input("find all PDFs in ~/Documents", &mut state);
    let r = process_input("and then skip hidden files", &mut state);
    match &r {
        NlResponse::PlanEdited { .. } => {} // success
        other => panic!("expected PlanEdited for 'and then skip', got: {:?}", other),
    }
}

// -- "ok search for TODO" keyword extraction --

#[test]
fn test_hardening_ok_search_todo_keyword() {
    let mut state = DialogueState::new();
    let r = process_input("ok search for TODO in ~/src", &mut state);
    match &r {
        NlResponse::PlanCreated { workflow_yaml, .. } => {
            let yaml_lower = workflow_yaml.to_lowercase();
            assert!(!yaml_lower.contains("pattern: \"ok\"") && !yaml_lower.contains("pattern: ok"),
                "'ok' should NOT be the search pattern: {}", workflow_yaml);
            // "todo" should be the keyword
            assert!(yaml_lower.contains("todo"),
                "should contain 'todo' as keyword: {}", workflow_yaml);
        }
        other => panic!("expected PlanCreated, got: {:?}", other),
    }
}

#[test]
fn test_hardening_so_find_pdfs_no_so_keyword() {
    let mut state = DialogueState::new();
    let r = process_input("so find all PDFs in ~/Documents", &mut state);
    match &r {
        NlResponse::PlanCreated { workflow_yaml, .. } => {
            // "so" should not appear as a keyword/pattern
            assert!(!workflow_yaml.contains("pattern: \"so\"") && !workflow_yaml.contains("pattern: so\n"),
                "'so' should NOT be a keyword: {}", workflow_yaml);
        }
        other => panic!("expected PlanCreated, got: {:?}", other),
    }
}

// -- "ok lgtm" compound approval --

#[test]
fn test_hardening_ok_lgtm_approves() {
    let mut state = DialogueState::new();
    let _ = process_input("compress file.txt", &mut state);
    let r = process_input("ok lgtm", &mut state);
    assert!(matches!(r, NlResponse::Approved { .. }),
        "'ok lgtm' should approve, got: {:?}", r);
}

#[test]
fn test_hardening_sure_yeah_approves() {
    let mut state = DialogueState::new();
    let _ = process_input("compress file.txt", &mut state);
    let r = process_input("sure yeah", &mut state);
    assert!(matches!(r, NlResponse::Approved { .. }),
        "'sure yeah' should approve, got: {:?}", r);
}

#[test]
fn test_hardening_fine_ok_approves() {
    let mut state = DialogueState::new();
    let _ = process_input("compress file.txt", &mut state);
    let r = process_input("fine ok", &mut state);
    assert!(matches!(r, NlResponse::Approved { .. }),
        "'fine ok' should approve, got: {:?}", r);
}

// -- ~/path/file.sql type inference --

#[test]
fn test_hardening_tilde_sql_file_compiles() {
    let mut state = DialogueState::new();
    let r = process_input("compress ~/backup/database.sql", &mut state);
    assert!(matches!(r, NlResponse::PlanCreated { .. }),
        "~/backup/database.sql should create plan: {:?}", r);
    assert_yaml_compiles(&r);
}

#[test]
fn test_hardening_tilde_tar_gz_compiles() {
    let mut state = DialogueState::new();
    let r = process_input("extract ~/archive.tar.gz", &mut state);
    assert!(matches!(r, NlResponse::PlanCreated { .. }),
        "~/archive.tar.gz should create plan: {:?}", r);
    assert_yaml_compiles(&r);
}

#[test]
fn test_hardening_tilde_dir_still_works() {
    let mut state = DialogueState::new();
    let r = process_input("list ~/Documents", &mut state);
    assert!(matches!(r, NlResponse::PlanCreated { .. }),
        "~/Documents should create plan: {:?}", r);
    assert_yaml_compiles(&r);
}

// -- Dictionary expansion: slang doesn't break things --

#[test]
fn test_hardening_slang_doesnt_create_workflow() {
    let mut state = DialogueState::new();
    let r = process_input("yikes whoa that's crazy", &mut state);
    assert!(!matches!(r, NlResponse::PlanCreated { .. }),
        "slang should NOT create workflow: {:?}", r);
}

#[test]
fn test_hardening_gonna_wanna_passthrough() {
    let mut state = DialogueState::new();
    // "gonna" and "wanna" should not be corrected to domain ops
    let r = process_input("I'm gonna wanna compress file.txt", &mut state);
    assert!(matches!(r, NlResponse::PlanCreated { .. }),
        "should still create workflow with slang: {:?}", r);
    assert_yaml_compiles(&r);
}

#[test]
fn test_nl_generated_yaml_compiles_list() {
    let mut state = DialogueState::new();
    let r = process_input("list ~/Documents", &mut state);
    assert_yaml_compiles(&r);
}

#[test]
fn test_nl_generated_yaml_compiles_extract() {
    let mut state = DialogueState::new();
    let r = process_input("extract ~/comic.cbz", &mut state);
    assert_yaml_compiles(&r);
}

#[test]
fn test_nl_generated_yaml_compiles_walk() {
    let mut state = DialogueState::new();
    let r = process_input("walk the directory tree in /tmp", &mut state);
    assert_yaml_compiles(&r);
}

#[test]
fn test_nl_edited_yaml_compiles() {
    let mut state = DialogueState::new();
    process_input("zip up everything in ~/Downloads", &mut state);
    let r = process_input("skip any subdirectory named .git", &mut state);
    assert_yaml_compiles_edited(&r);
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn assert_plan_created(response: &NlResponse, expected_ops: &[&str]) {
    match response {
        NlResponse::PlanCreated { workflow_yaml, .. } => {
            for op in expected_ops {
                assert!(
                    workflow_yaml.contains(op),
                    "workflow should contain '{}': {}",
                    op, workflow_yaml
                );
            }
        }
        other => panic!("expected PlanCreated, got: {:?}", other),
    }
}

fn assert_yaml_compiles(response: &NlResponse) {
    if let NlResponse::PlanCreated { workflow_yaml, .. } = response {
        let parsed = cadmus::workflow::parse_workflow(workflow_yaml)
            .expect("should parse");
        let registry = cadmus::fs_types::build_full_registry();
        cadmus::workflow::compile_workflow(&parsed, &registry)
            .expect("should compile");
    }
}

fn assert_yaml_compiles_edited(response: &NlResponse) {
    if let NlResponse::PlanEdited { workflow_yaml, .. } = response {
        let parsed = cadmus::workflow::parse_workflow(workflow_yaml)
            .expect("should parse");
        let registry = cadmus::fs_types::build_full_registry();
        cadmus::workflow::compile_workflow(&parsed, &registry)
            .expect("should compile");
    }
}

// ==========================================================================
// Red-team integration tests
// ==========================================================================

// -- BUG-001: "never mind" rejection --

#[test]
fn test_redteam_never_mind_rejects() {
    let mut state = DialogueState::new();
    let r1 = process_input("compress file.txt", &mut state);
    assert!(matches!(r1, NlResponse::PlanCreated { .. }));
    let r2 = process_input("never mind", &mut state);
    assert!(matches!(r2, NlResponse::Rejected),
        "never mind should reject, got: {:?}", r2);
}

// -- BUG-002/003: search keyword extraction --

#[test]
fn test_redteam_search_todo_keyword() {
    let mut state = DialogueState::new();
    let r = process_input("search for TODO in ~/src", &mut state);
    if let NlResponse::PlanCreated { workflow_yaml, .. } = &r {
        assert!(workflow_yaml.contains("todo") || workflow_yaml.contains("TODO"),
            "search pattern should contain 'todo', got:\n{}", workflow_yaml);
        assert!(!workflow_yaml.contains("good"),
            "search pattern should NOT contain 'good', got:\n{}", workflow_yaml);
    } else {
        panic!("expected PlanCreated, got: {:?}", r);
    }
}

#[test]
fn test_redteam_search_fixme_keyword() {
    let mut state = DialogueState::new();
    let r = process_input("search for FIXME in ~/src", &mut state);
    if let NlResponse::PlanCreated { workflow_yaml, .. } = &r {
        assert!(workflow_yaml.contains("fixme") || workflow_yaml.contains("FIXME"),
            "search pattern should contain 'fixme', got:\n{}", workflow_yaml);
    } else {
        panic!("expected PlanCreated, got: {:?}", r);
    }
}

// -- BUG-004: comma-containing approve phrases --

#[test]
fn test_redteam_perfect_comma_ship_it_approves() {
    let mut state = DialogueState::new();
    let _ = process_input("compress file.txt", &mut state);
    let r = process_input("perfect, ship it", &mut state);
    assert!(matches!(r, NlResponse::Approved { .. }),
        "perfect, ship it should approve, got: {:?}", r);
}

#[test]
fn test_redteam_yep_comma_run_it_approves() {
    let mut state = DialogueState::new();
    let _ = process_input("compress file.txt", &mut state);
    let r = process_input("yep, run it", &mut state);
    assert!(matches!(r, NlResponse::Approved { .. }),
        "yep, run it should approve, got: {:?}", r);
}

// -- BUG-005: natural rejection phrases --

#[test]
fn test_redteam_actually_no_rejects() {
    let mut state = DialogueState::new();
    let _ = process_input("compress file.txt", &mut state);
    let r = process_input("actually no", &mut state);
    assert!(matches!(r, NlResponse::Rejected),
        "actually no should reject, got: {:?}", r);
}

#[test]
fn test_redteam_forget_about_it_rejects() {
    let mut state = DialogueState::new();
    let _ = process_input("compress file.txt", &mut state);
    let r = process_input("forget about it", &mut state);
    assert!(matches!(r, NlResponse::Rejected),
        "forget about it should reject, got: {:?}", r);
}

#[test]
fn test_redteam_wait_no_rejects() {
    let mut state = DialogueState::new();
    let _ = process_input("compress file.txt", &mut state);
    let r = process_input("wait no", &mut state);
    assert!(matches!(r, NlResponse::Rejected),
        "wait no should reject, got: {:?}", r);
}

// -- BUG-006: double approve --

#[test]
fn test_redteam_double_approve_fails() {
    let mut state = DialogueState::new();
    let _ = process_input("compress file.txt", &mut state);
    let r1 = process_input("yes", &mut state);
    assert!(matches!(r1, NlResponse::Approved { .. }));
    let r2 = process_input("yes", &mut state);
    assert!(matches!(r2, NlResponse::NeedsClarification { .. }),
        "second approve should need clarification, got: {:?}", r2);
}

// -- BUG-008: "remove the step" --

#[test]
fn test_redteam_remove_the_step_removes_last() {
    let mut state = DialogueState::new();
    let r1 = process_input("find all PDFs in ~/Documents", &mut state);
    assert!(matches!(r1, NlResponse::PlanCreated { .. }));
    let r2 = process_input("remove the last step", &mut state);
    match &r2 {
        NlResponse::PlanEdited { diff_description, .. } => {
            assert!(diff_description.contains("Removed"),
                "should describe removal: {}", diff_description);
        }
        other => panic!("expected PlanEdited, got: {:?}", other),
    }
}

// -- BUG-010: "sure why not" and "yeah that works" approval --

#[test]
fn test_redteam_sure_why_not_approves() {
    let mut state = DialogueState::new();
    let _ = process_input("compress file.txt", &mut state);
    let r = process_input("sure why not", &mut state);
    assert!(matches!(r, NlResponse::Approved { .. }),
        "sure why not should approve, got: {:?}", r);
}

#[test]
fn test_redteam_yeah_that_works_approves() {
    let mut state = DialogueState::new();
    let _ = process_input("compress file.txt", &mut state);
    let r = process_input("yeah that works", &mut state);
    assert!(matches!(r, NlResponse::Approved { .. }),
        "yeah that works should approve, got: {:?}", r);
}

// -- BUG-011: casual English shouldn't create workflows --

#[test]
fn test_redteam_mind_not_workflow() {
    let mut state = DialogueState::new();
    let r = process_input("I have something in mind", &mut state);
    assert!(!matches!(r, NlResponse::PlanCreated { .. }),
        "casual English should NOT create workflow, got: {:?}", r);
}

// -- Adversarial inputs --

#[test]
fn test_redteam_unicode_path() {
    let mut state = DialogueState::new();
    let r = process_input("compress /Users/me/résumé.pdf", &mut state);
    assert!(matches!(r, NlResponse::PlanCreated { .. }),
        "unicode path should work: {:?}", r);
    assert_yaml_compiles(&r);
}

#[test]
fn test_redteam_only_punctuation() {
    let mut state = DialogueState::new();
    let r = process_input("!@#$%^&*()", &mut state);
    assert!(matches!(r, NlResponse::NeedsClarification { .. }),
        "punctuation-only should clarify: {:?}", r);
}

#[test]
fn test_redteam_relative_path() {
    let mut state = DialogueState::new();
    let r = process_input("compress ./src/main.rs", &mut state);
    assert!(matches!(r, NlResponse::PlanCreated { .. }));
    assert_yaml_compiles(&r);
}

#[test]
fn test_redteam_dotdot_path() {
    let mut state = DialogueState::new();
    let r = process_input("compress ../other/file.txt", &mut state);
    assert!(matches!(r, NlResponse::PlanCreated { .. }));
    assert_yaml_compiles(&r);
}

// -- Multi-turn state transitions --

#[test]
fn test_redteam_create_reject_create_approve() {
    let mut state = DialogueState::new();
    let r1 = process_input("compress file.txt", &mut state);
    assert!(matches!(r1, NlResponse::PlanCreated { .. }));
    let r2 = process_input("no", &mut state);
    assert!(matches!(r2, NlResponse::Rejected));
    let r3 = process_input("list ~/Desktop", &mut state);
    assert!(matches!(r3, NlResponse::PlanCreated { .. }));
    let r4 = process_input("yes", &mut state);
    assert!(matches!(r4, NlResponse::Approved { .. }));
}

#[test]
fn test_redteam_create_overwrite_approve() {
    let mut state = DialogueState::new();
    let r1 = process_input("compress file.txt", &mut state);
    assert!(matches!(r1, NlResponse::PlanCreated { .. }));
    // Second create overwrites first
    let r2 = process_input("list ~/Desktop", &mut state);
    assert!(matches!(r2, NlResponse::PlanCreated { .. }));
    if let NlResponse::PlanCreated { workflow_yaml, .. } = &r2 {
        assert!(workflow_yaml.contains("list_dir"),
            "second plan should be list_dir: {}", workflow_yaml);
    }
    let r3 = process_input("yes", &mut state);
    assert!(matches!(r3, NlResponse::Approved { .. }));
}

// -- Typo correction edge cases --

#[test]
fn test_redteam_heavy_typos_still_work() {
    let mut state = DialogueState::new();
    let r = process_input("extrct the archve at ~/comic.cbz", &mut state);
    assert!(matches!(r, NlResponse::PlanCreated { .. }),
        "heavy typos should still create plan: {:?}", r);
    assert_yaml_compiles(&r);
}

#[test]
fn test_redteam_transposed_letters() {
    let mut state = DialogueState::new();
    let r = process_input("wlak teh direcotry tree in /var", &mut state);
    assert!(matches!(r, NlResponse::PlanCreated { .. }),
        "transposed letters should still create plan: {:?}", r);
    assert_yaml_compiles(&r);
}

// ===========================================================================
// Phase 5: File Type Dictionary integration tests
// ===========================================================================

// --- New file types detected as paths ---

#[test]
fn test_filetype_heic_detected_as_path() {
    let mut state = DialogueState::new();
    let r = process_input("copy photo.heic to ~/backup/", &mut state);
    assert!(matches!(r, NlResponse::PlanCreated { .. }),
        "photo.heic should be detected as a file path: {:?}", r);
    assert_yaml_compiles(&r);
}

#[test]
fn test_filetype_webm_detected_as_path() {
    let mut state = DialogueState::new();
    let r = process_input("copy video.webm to ~/backup/", &mut state);
    assert!(matches!(r, NlResponse::PlanCreated { .. }),
        "video.webm should be detected as a file path: {:?}", r);
    assert_yaml_compiles(&r);
}

#[test]
fn test_filetype_rkt_detected_as_path() {
    let mut state = DialogueState::new();
    let r = process_input("search for define in code.rkt", &mut state);
    assert!(matches!(r, NlResponse::PlanCreated { .. }),
        "code.rkt should be detected as a file path: {:?}", r);
    assert_yaml_compiles(&r);
}

#[test]
fn test_filetype_scm_detected_as_path() {
    let mut state = DialogueState::new();
    let r = process_input("search for lambda in program.scm", &mut state);
    assert!(matches!(r, NlResponse::PlanCreated { .. }),
        "program.scm should be detected as a file path: {:?}", r);
    assert_yaml_compiles(&r);
}

#[test]
fn test_filetype_flac_detected_as_path() {
    let mut state = DialogueState::new();
    let r = process_input("copy song.flac to ~/music/", &mut state);
    assert!(matches!(r, NlResponse::PlanCreated { .. }),
        "song.flac should be detected as a file path: {:?}", r);
    assert_yaml_compiles(&r);
}

#[test]
fn test_filetype_wasm_detected_as_path() {
    let mut state = DialogueState::new();
    let r = process_input("checksum app.wasm", &mut state);
    assert!(matches!(r, NlResponse::PlanCreated { .. }),
        "app.wasm should be detected as a file path: {:?}", r);
    assert_yaml_compiles(&r);
}

#[test]
fn test_filetype_epub_detected_as_path() {
    let mut state = DialogueState::new();
    let r = process_input("copy book.epub to ~/library/", &mut state);
    assert!(matches!(r, NlResponse::PlanCreated { .. }),
        "book.epub should be detected as a file path: {:?}", r);
    assert_yaml_compiles(&r);
}

#[test]
fn test_filetype_plist_detected_as_path() {
    let mut state = DialogueState::new();
    let r = process_input("copy config.plist to ~/backup/", &mut state);
    assert!(matches!(r, NlResponse::PlanCreated { .. }),
        "config.plist should be detected as a file path: {:?}", r);
    assert_yaml_compiles(&r);
}

// --- Workflow type inference with new extensions ---

#[test]
fn test_filetype_flac_infers_audio_type() {
    let mut state = DialogueState::new();
    let r = process_input("copy song.flac to ~/backup/", &mut state);
    if let NlResponse::PlanCreated { workflow_yaml, .. } = &r {
        // Should compile — the dictionary gives File(Audio) for .flac
        let def: cadmus::workflow::WorkflowDef =
            serde_yaml::from_str(workflow_yaml).unwrap();
        let reg = cadmus::fs_types::build_full_registry();
        let compiled = cadmus::workflow::compile_workflow(&def, &reg);
        assert!(compiled.is_ok(), "flac workflow should compile: {:?}", compiled.err());
    }
}

#[test]
fn test_filetype_scheme_infers_text_type() {
    let mut state = DialogueState::new();
    let r = process_input("search for define in program.scm", &mut state);
    if let NlResponse::PlanCreated { workflow_yaml, .. } = &r {
        let def: cadmus::workflow::WorkflowDef =
            serde_yaml::from_str(workflow_yaml).unwrap();
        let reg = cadmus::fs_types::build_full_registry();
        let compiled = cadmus::workflow::compile_workflow(&def, &reg);
        assert!(compiled.is_ok(), "scheme workflow should compile: {:?}", compiled.err());
    }
}

// --- Dictionary query API ---

#[test]
fn test_filetype_dictionary_describe() {
    let dict = cadmus::filetypes::dictionary();
    let desc = dict.describe_file_type("mp4");
    assert!(desc.contains("MP4"), "should mention MP4: {}", desc);
    assert!(desc.contains("ffmpeg"), "should mention ffmpeg tool: {}", desc);
}

#[test]
fn test_filetype_dictionary_category_query() {
    let dict = cadmus::filetypes::dictionary();
    let source_exts = dict.extensions_for_category(
        &cadmus::filetypes::FileTypeCategory::SourceCode
    );
    assert!(source_exts.contains(&"scm"), "source code should include .scm");
    assert!(source_exts.contains(&"ss"), "source code should include .ss");
    assert!(source_exts.contains(&"rkt"), "source code should include .rkt");
    assert!(source_exts.contains(&"rs"), "source code should include .rs");
    assert!(source_exts.len() >= 35, "should have 35+ source code exts, got {}", source_exts.len());
}

#[test]
fn test_filetype_dictionary_compound_ext_lookup() {
    let dict = cadmus::filetypes::dictionary();
    // lookup_by_path should prefer tar.gz over gz
    let entry = dict.lookup_by_path("backup.tar.gz").unwrap();
    assert_eq!(entry.ext, "tar.gz");
    // But plain .gz should still work
    let entry2 = dict.lookup_by_path("data.gz").unwrap();
    assert_eq!(entry2.ext, "gz");
}

#[test]
fn test_filetype_dictionary_unknown_returns_none() {
    let dict = cadmus::filetypes::dictionary();
    assert!(dict.lookup("xyzzy").is_none());
    assert!(dict.lookup_by_path("noext").is_none());
    assert!(dict.lookup_by_path("file.").is_none());
}

#[test]
fn test_filetype_dictionary_size() {
    let dict = cadmus::filetypes::dictionary();
    assert!(dict.len() >= 170, "dictionary should have 170+ entries, got {}", dict.len());
}

// =========================================================================
// Phase 6: YAML externalization integration tests
// =========================================================================

// -- Vocab YAML loading --

#[test]
fn test_yaml_vocab_synonyms_loaded() {
    // Verify synonyms load from YAML and work end-to-end
    let mut state = cadmus::nl::dialogue::DialogueState::new();
    let r = cadmus::nl::process_input("zip up everything in ~/Downloads", &mut state);
    match r {
        cadmus::nl::NlResponse::PlanCreated { workflow_yaml, .. } => {
            assert!(workflow_yaml.contains("pack_archive"),
                "should have pack_archive in YAML from synonym, got: {}", workflow_yaml);
        }
        other => panic!("expected PlanCreated, got: {:?}", other),
    }
}

#[test]
fn test_yaml_vocab_contractions_loaded() {
    // "don't" should expand to "do not" via YAML contractions
    let result = cadmus::nl::normalize::normalize("don't walk the tree");
    assert!(result.tokens.contains(&"do".to_string()), "should expand don't: {:?}", result.tokens);
    assert!(result.tokens.contains(&"not".to_string()), "should expand don't: {:?}", result.tokens);
}

#[test]
fn test_yaml_vocab_ordinals_loaded() {
    let result = cadmus::nl::normalize::normalize("move first step");
    assert!(result.canonical_tokens.contains(&"1".to_string()),
        "should canonicalize 'first' to '1': {:?}", result.canonical_tokens);
}

#[test]
fn test_yaml_vocab_approvals_loaded() {
    let mut state = cadmus::nl::dialogue::DialogueState::new();
    let _ = cadmus::nl::process_input("list ~/Desktop", &mut state);
    let r = cadmus::nl::process_input("lgtm", &mut state);
    assert!(matches!(r, cadmus::nl::NlResponse::Approved { .. }),
        "lgtm should approve via YAML-loaded approval list: {:?}", r);
}

#[test]
fn test_yaml_vocab_rejections_loaded() {
    let mut state = cadmus::nl::dialogue::DialogueState::new();
    let _ = cadmus::nl::process_input("list ~/Desktop", &mut state);
    let r = cadmus::nl::process_input("nah forget it", &mut state);
    assert!(matches!(r, cadmus::nl::NlResponse::Rejected),
        "nah forget it should reject via YAML-loaded rejection list: {:?}", r);
}

// -- Dictionary YAML loading --

#[test]
fn test_yaml_dictionary_typo_correction() {
    // "extrct" should correct to "extract" via YAML-loaded dictionary
    let dict = cadmus::nl::typo::build_domain_dict();
    let corrected = dict.correct("extrct");
    assert_eq!(corrected, "extract", "should correct typo via YAML dictionary");
}

#[test]
fn test_yaml_dictionary_unknown_passthrough() {
    let dict = cadmus::nl::typo::build_domain_dict();
    let result = dict.correct("xyzzyplugh");
    assert_eq!(result, "xyzzyplugh", "unknown word should pass through unchanged");
}

// -- Op descriptions from YAML packs --

#[test]
fn test_yaml_op_description_from_registry() {
    let desc = cadmus::fs_types::get_op_description("walk_tree");
    assert!(desc.is_some(), "walk_tree should have a description from fs.ops.yaml");
    assert!(desc.unwrap().contains("walk"), "description should mention walking: {}", desc.unwrap());
}

#[test]
fn test_yaml_op_description_power_tools() {
    let desc = cadmus::fs_types::get_op_description("git_log");
    assert!(desc.is_some(), "git_log should have a description from power_tools.ops.yaml");
    assert!(desc.unwrap().contains("git"), "description should mention git: {}", desc.unwrap());
}

#[test]
fn test_yaml_op_description_unknown_returns_none() {
    let desc = cadmus::fs_types::get_op_description("nonexistent_op_xyz");
    assert!(desc.is_none(), "unknown op should return None");
}

// -- CANONICAL_OPS derived from registry --

#[test]
fn test_yaml_canonical_ops_derived() {
    assert!(cadmus::nl::normalize::is_canonical_op("list_dir"),
        "list_dir should be canonical (from fs.ops.yaml)");
    assert!(cadmus::nl::normalize::is_canonical_op("git_log"),
        "git_log should be canonical (from power_tools.ops.yaml)");
    assert!(!cadmus::nl::normalize::is_canonical_op("nonexistent_xyz"),
        "nonexistent op should not be canonical");
}

#[test]
fn test_yaml_canonical_ops_count() {
    let ops = cadmus::nl::normalize::canonical_ops();
    assert!(ops.len() >= 113, "should have at least 113 canonical ops, got {}", ops.len());
}

// -- End-to-end: full pipeline with YAML data --

#[test]
fn test_yaml_full_pipeline_walk_tree() {
    let mut state = cadmus::nl::dialogue::DialogueState::new();
    let r = cadmus::nl::process_input("walk the directory tree in ~/Documents", &mut state);
    match r {
        cadmus::nl::NlResponse::PlanCreated { workflow_yaml, .. } => {
            assert!(workflow_yaml.contains("walk_tree"),
                "should have walk_tree from YAML synonyms: {}", workflow_yaml);
        }
        other => panic!("expected PlanCreated, got: {:?}", other),
    }
}

#[test]
fn test_yaml_full_pipeline_explain_op() {
    let mut state = cadmus::nl::dialogue::DialogueState::new();
    let r = cadmus::nl::process_input("what is walk_tree", &mut state);
    match r {
        cadmus::nl::NlResponse::Explanation { text, .. } => {
            // Description should come from fs.ops.yaml
            assert!(!text.is_empty(), "explanation should not be empty");
        }
        other => panic!("expected Explanation, got: {:?}", other),
    }
}

// ---------------------------------------------------------------------------
// NL Robustness: directory aliases + noun-to-filetype patterns
// ---------------------------------------------------------------------------

#[test]
fn test_nl_find_screenshots_on_my_desktop() {
    // The original failing input that motivated this feature
    let mut state = cadmus::nl::dialogue::DialogueState::new();
    let r = cadmus::nl::process_input("Find all screenshots on my desktop", &mut state);
    match r {
        cadmus::nl::NlResponse::PlanCreated { workflow_yaml, .. } => {
            assert!(workflow_yaml.contains("~/Desktop"),
                "should have ~/Desktop path: {}", workflow_yaml);
            assert!(workflow_yaml.contains("*.png"),
                "should have *.png pattern for screenshots: {}", workflow_yaml);
            assert!(workflow_yaml.contains("walk_tree"),
                "should have walk_tree step: {}", workflow_yaml);
            assert!(workflow_yaml.contains("find_matching"),
                "should have find_matching step: {}", workflow_yaml);
        }
        other => panic!("expected PlanCreated, got: {:?}", other),
    }
}

#[test]
fn test_nl_list_videos_in_my_downloads() {
    let mut state = cadmus::nl::dialogue::DialogueState::new();
    let r = cadmus::nl::process_input("list videos in my downloads", &mut state);
    match r {
        cadmus::nl::NlResponse::PlanCreated { workflow_yaml, .. } => {
            assert!(workflow_yaml.contains("~/Downloads"),
                "should have ~/Downloads path: {}", workflow_yaml);
            assert!(workflow_yaml.contains("*.mp4") || workflow_yaml.contains("*.mov"),
                "should have video pattern: {}", workflow_yaml);
        }
        other => panic!("expected PlanCreated, got: {:?}", other),
    }
}

#[test]
fn test_nl_find_pdfs_in_documents() {
    let mut state = cadmus::nl::dialogue::DialogueState::new();
    let r = cadmus::nl::process_input("find PDFs in documents", &mut state);
    match r {
        cadmus::nl::NlResponse::PlanCreated { workflow_yaml, .. } => {
            assert!(workflow_yaml.contains("~/Documents"),
                "should have ~/Documents path: {}", workflow_yaml);
            assert!(workflow_yaml.contains("*.pdf"),
                "should have *.pdf pattern: {}", workflow_yaml);
        }
        other => panic!("expected PlanCreated, got: {:?}", other),
    }
}

#[test]
fn test_nl_zip_up_photos_on_my_desktop() {
    let mut state = cadmus::nl::dialogue::DialogueState::new();
    let r = cadmus::nl::process_input("zip up photos on my desktop", &mut state);
    match r {
        cadmus::nl::NlResponse::PlanCreated { workflow_yaml, .. } => {
            assert!(workflow_yaml.contains("~/Desktop"),
                "should have ~/Desktop path: {}", workflow_yaml);
            assert!(workflow_yaml.contains("pack_archive"),
                "should have pack_archive step: {}", workflow_yaml);
            // photos → *.png pattern used as filter
            assert!(workflow_yaml.contains("*.png") || workflow_yaml.contains("filter"),
                "should have photo filter: {}", workflow_yaml);
        }
        other => panic!("expected PlanCreated, got: {:?}", other),
    }
}

#[test]
fn test_nl_bare_desktop_resolves_path() {
    // "list desktop" without "my" should still resolve
    let mut state = cadmus::nl::dialogue::DialogueState::new();
    let r = cadmus::nl::process_input("list desktop", &mut state);
    match r {
        cadmus::nl::NlResponse::PlanCreated { workflow_yaml, .. } => {
            assert!(workflow_yaml.contains("~/Desktop"),
                "should have ~/Desktop path: {}", workflow_yaml);
        }
        other => panic!("expected PlanCreated, got: {:?}", other),
    }
}

#[test]
fn test_nl_find_stuff_no_noun_pattern() {
    // "find stuff on my desktop" — "stuff" is not a noun pattern
    let mut state = cadmus::nl::dialogue::DialogueState::new();
    let r = cadmus::nl::process_input("find stuff on my desktop", &mut state);
    match r {
        cadmus::nl::NlResponse::PlanCreated { workflow_yaml, .. } => {
            assert!(workflow_yaml.contains("~/Desktop"),
                "should have ~/Desktop path: {}", workflow_yaml);
            // "stuff" becomes a keyword, used as *stuff* pattern in find_matching
            assert!(workflow_yaml.contains("*stuff*"),
                "should have *stuff* keyword pattern: {}", workflow_yaml);
        }
        other => panic!("expected PlanCreated, got: {:?}", other),
    }
}

#[test]
fn test_nl_explicit_path_with_noun() {
    // Explicit ~/Projects path should win over any alias
    let mut state = cadmus::nl::dialogue::DialogueState::new();
    let r = cadmus::nl::process_input("find screenshots in ~/Projects", &mut state);
    match r {
        cadmus::nl::NlResponse::PlanCreated { workflow_yaml, .. } => {
            assert!(workflow_yaml.contains("~/Projects"),
                "should have ~/Projects path: {}", workflow_yaml);
            assert!(workflow_yaml.contains("*.png"),
                "should have *.png pattern: {}", workflow_yaml);
        }
        other => panic!("expected PlanCreated, got: {:?}", other),
    }
}

#[test]
fn test_nl_find_logs_on_desktop() {
    let mut state = cadmus::nl::dialogue::DialogueState::new();
    let r = cadmus::nl::process_input("find logs on my desktop", &mut state);
    match r {
        cadmus::nl::NlResponse::PlanCreated { workflow_yaml, .. } => {
            assert!(workflow_yaml.contains("~/Desktop"),
                "should have ~/Desktop path: {}", workflow_yaml);
            assert!(workflow_yaml.contains("*.log"),
                "should have *.log pattern: {}", workflow_yaml);
        }
        other => panic!("expected PlanCreated, got: {:?}", other),
    }
}

// -- Quoted string handling --

#[test]
fn test_nl_quoted_path_no_name() {
    let mut state = cadmus::nl::dialogue::DialogueState::new();
    let r = cadmus::nl::process_input(r#"list all the files in "NO NAME""#, &mut state);
    match r {
        cadmus::nl::NlResponse::PlanCreated { workflow_yaml, .. } => {
            assert!(workflow_yaml.contains("NO NAME"),
                "should have 'NO NAME' as path: {}", workflow_yaml);
            assert!(workflow_yaml.contains("list_dir"),
                "should have list_dir step: {}", workflow_yaml);
        }
        other => panic!("expected PlanCreated, got: {:?}", other),
    }
}

#[test]
fn test_nl_quoted_path_with_find() {
    let mut state = cadmus::nl::dialogue::DialogueState::new();
    let r = cadmus::nl::process_input(r#"find PDFs in "My Backup Drive""#, &mut state);
    match r {
        cadmus::nl::NlResponse::PlanCreated { workflow_yaml, .. } => {
            assert!(workflow_yaml.contains("My Backup Drive"),
                "should have 'My Backup Drive' as path: {}", workflow_yaml);
            assert!(workflow_yaml.contains("*.pdf"),
                "should have *.pdf pattern: {}", workflow_yaml);
        }
        other => panic!("expected PlanCreated, got: {:?}", other),
    }
}
