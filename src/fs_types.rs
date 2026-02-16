use crate::registry::{AlgebraicProperties, OperationRegistry, PolyOpSignature};
use crate::type_expr::TypeExpr;

// ---------------------------------------------------------------------------
// Filesystem type vocabulary
// ---------------------------------------------------------------------------
//
// Primitives:  Path, Bytes, Name, Text, Line, Pattern, Metadata
// Constructors: File(content), Dir(entry), Archive(content, format),
//               Seq(elem), Entry(key, val), Match(pattern, val), Tree(node)
// Format tags:  Zip, Cbz, Tar, TarGz

/// Build and return an OperationRegistry populated with the filesystem
/// type vocabulary: ~15 polymorphic operations covering directory listing,
/// file I/O, archive manipulation, search, filtering, and renaming.
pub fn build_fs_registry() -> OperationRegistry {
    let mut reg = OperationRegistry::new();

    // 1. list_dir<a>: Dir(a) → Seq(Entry(Name, a))
    //    Lists directory contents as name-value pairs.
    reg.register_poly(
        "list_dir",
        PolyOpSignature::new(
            vec!["a".into()],
            vec![TypeExpr::dir(TypeExpr::var("a"))],
            TypeExpr::seq(TypeExpr::entry(TypeExpr::prim("Name"), TypeExpr::var("a"))),
        ),
        AlgebraicProperties::none(),
        "ls — list directory contents",
    );

    // 2. read_file<a>: File(a) → a
    //    Read file contents.
    reg.register_poly(
        "read_file",
        PolyOpSignature::new(
            vec!["a".into()],
            vec![TypeExpr::file(TypeExpr::var("a"))],
            TypeExpr::var("a"),
        ),
        AlgebraicProperties::none(),
        "cat — read file contents",
    );

    // 3. write_file<a>: a, Path → File(a)
    //    Write content to a file at the given path.
    reg.register_poly(
        "write_file",
        PolyOpSignature::new(
            vec!["a".into()],
            vec![TypeExpr::var("a"), TypeExpr::prim("Path")],
            TypeExpr::file(TypeExpr::var("a")),
        ),
        AlgebraicProperties::none(),
        "write content to file at path",
    );

    // 4. stat: Path → Metadata
    //    Get file/directory metadata (size, mtime, permissions).
    reg.register_poly(
        "stat",
        PolyOpSignature::mono(
            vec![TypeExpr::prim("Path")],
            TypeExpr::prim("Metadata"),
        ),
        AlgebraicProperties { idempotent: true, ..Default::default() },
        "stat — get file metadata",
    );

    // 5. walk_tree<a>: Dir(a) → Tree(Entry(Name, a))
    //    Recursively walk a directory tree.
    reg.register_poly(
        "walk_tree",
        PolyOpSignature::new(
            vec!["a".into()],
            vec![TypeExpr::dir(TypeExpr::var("a"))],
            TypeExpr::tree(TypeExpr::entry(TypeExpr::prim("Name"), TypeExpr::var("a"))),
        ),
        AlgebraicProperties::none(),
        "find — recursively walk directory tree",
    );

    // 6. filter<a>: Seq(a), Pattern → Seq(a)
    //    Filter a sequence by a pattern. Idempotent.
    reg.register_poly(
        "filter",
        PolyOpSignature::new(
            vec!["a".into()],
            vec![TypeExpr::seq(TypeExpr::var("a")), TypeExpr::prim("Pattern")],
            TypeExpr::seq(TypeExpr::var("a")),
        ),
        AlgebraicProperties { idempotent: true, ..Default::default() },
        "grep/filter — filter sequence by pattern",
    );

    // 7. sort_by<a>: Seq(a) → Seq(a)
    //    Sort a sequence. Idempotent.
    reg.register_poly(
        "sort_by",
        PolyOpSignature::new(
            vec!["a".into()],
            vec![TypeExpr::seq(TypeExpr::var("a"))],
            TypeExpr::seq(TypeExpr::var("a")),
        ),
        AlgebraicProperties { idempotent: true, ..Default::default() },
        "sort — sort sequence elements",
    );

    // 8. extract_archive<a, fmt>: File(Archive(a, fmt)) → Seq(Entry(Name, a))
    //    Extract archive contents.
    reg.register_poly(
        "extract_archive",
        PolyOpSignature::new(
            vec!["a".into(), "fmt".into()],
            vec![TypeExpr::file(TypeExpr::archive(TypeExpr::var("a"), TypeExpr::var("fmt")))],
            TypeExpr::seq(TypeExpr::entry(TypeExpr::prim("Name"), TypeExpr::var("a"))),
        ),
        AlgebraicProperties::none(),
        "unzip/tar -x — extract archive contents",
    );

    // 9. pack_archive<a, fmt>: Seq(Entry(Name, a)), fmt → File(Archive(a, fmt))
    //    Pack entries into an archive.
    reg.register_poly(
        "pack_archive",
        PolyOpSignature::new(
            vec!["a".into(), "fmt".into()],
            vec![
                TypeExpr::seq(TypeExpr::entry(TypeExpr::prim("Name"), TypeExpr::var("a"))),
                TypeExpr::var("fmt"),
            ],
            TypeExpr::file(TypeExpr::archive(TypeExpr::var("a"), TypeExpr::var("fmt"))),
        ),
        AlgebraicProperties::none(),
        "zip/tar -c — pack entries into archive",
    );

    // 10. concat_seq<a>: Seq(a), Seq(a) → Seq(a)
    //     Concatenate two sequences. Associative but NOT commutative (order matters).
    reg.register_poly(
        "concat_seq",
        PolyOpSignature::new(
            vec!["a".into()],
            vec![TypeExpr::seq(TypeExpr::var("a")), TypeExpr::seq(TypeExpr::var("a"))],
            TypeExpr::seq(TypeExpr::var("a")),
        ),
        AlgebraicProperties {
            associative: true,
            commutative: false,
            identity: Some("[]".to_string()),
            ..Default::default()
        },
        "cat/concat — concatenate sequences (order-preserving)",
    );

    // 11. rename: Entry(Name, a), Name → Entry(Name, a)
    //     Rename an entry (change its name, keep its value).
    reg.register_poly(
        "rename",
        PolyOpSignature::new(
            vec!["a".into()],
            vec![
                TypeExpr::entry(TypeExpr::prim("Name"), TypeExpr::var("a")),
                TypeExpr::prim("Name"),
            ],
            TypeExpr::entry(TypeExpr::prim("Name"), TypeExpr::var("a")),
        ),
        AlgebraicProperties::none(),
        "mv — rename entry",
    );

    // 12. move_entry: Entry(Name, a), Path → Entry(Name, a)
    //     Move an entry to a new location.
    reg.register_poly(
        "move_entry",
        PolyOpSignature::new(
            vec!["a".into()],
            vec![
                TypeExpr::entry(TypeExpr::prim("Name"), TypeExpr::var("a")),
                TypeExpr::prim("Path"),
            ],
            TypeExpr::entry(TypeExpr::prim("Name"), TypeExpr::var("a")),
        ),
        AlgebraicProperties::none(),
        "mv — move entry to new path",
    );

    // 13. search_content: Seq(Entry(Name, File(Text))), Pattern → Seq(Match(Pattern, Line))
    //     Search file contents for a pattern.
    reg.register_poly(
        "search_content",
        PolyOpSignature::mono(
            vec![
                TypeExpr::seq(TypeExpr::entry(
                    TypeExpr::prim("Name"),
                    TypeExpr::file(TypeExpr::prim("Text")),
                )),
                TypeExpr::prim("Pattern"),
            ],
            TypeExpr::seq(TypeExpr::match_type(TypeExpr::prim("Pattern"), TypeExpr::prim("Line"))),
        ),
        AlgebraicProperties::none(),
        "grep — search file contents for pattern",
    );

    // 14. find_matching<a>: Pattern, Seq(Entry(Name, a)) → Seq(Entry(Name, a))
    //     Filter entries whose names match a pattern.
    reg.register_poly(
        "find_matching",
        PolyOpSignature::new(
            vec!["a".into()],
            vec![
                TypeExpr::prim("Pattern"),
                TypeExpr::seq(TypeExpr::entry(TypeExpr::prim("Name"), TypeExpr::var("a"))),
            ],
            TypeExpr::seq(TypeExpr::entry(TypeExpr::prim("Name"), TypeExpr::var("a"))),
        ),
        AlgebraicProperties { idempotent: true, ..Default::default() },
        "find -name — filter entries by name pattern",
    );

    // 15. map_entries<a, b>: Seq(Entry(Name, a)) → Seq(Entry(Name, b))
    //     Transform entry values (requires an op a→b to be available).
    reg.register_poly(
        "map_entries",
        PolyOpSignature::new(
            vec!["a".into(), "b".into()],
            vec![TypeExpr::seq(TypeExpr::entry(TypeExpr::prim("Name"), TypeExpr::var("a")))],
            TypeExpr::seq(TypeExpr::entry(TypeExpr::prim("Name"), TypeExpr::var("b"))),
        ),
        AlgebraicProperties::none(),
        "xargs/map — transform entry values",
    );

    reg
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;


    #[test]
    fn test_all_ops_registered() {
        let reg = build_fs_registry();
        let names = reg.poly_op_names();
        assert!(names.len() >= 15, "expected at least 15 ops, got {}", names.len());

        let expected = vec![
            "list_dir", "read_file", "write_file", "stat", "walk_tree",
            "filter", "sort_by", "extract_archive", "pack_archive",
            "concat_seq", "rename", "move_entry", "search_content",
            "find_matching", "map_entries",
        ];
        for name in &expected {
            assert!(
                reg.get_poly(name).is_some(),
                "missing op: {}",
                name
            );
        }
    }

    #[test]
    fn test_ops_lookupable_by_output() {
        let reg = build_fs_registry();

        // list_dir produces Seq(Entry(Name, Bytes))
        let target = TypeExpr::seq(TypeExpr::entry(
            TypeExpr::prim("Name"),
            TypeExpr::prim("Bytes"),
        ));
        let matches = reg.ops_for_output_expr(&target);
        let names: Vec<&str> = matches.iter().map(|m| m.op.name.as_str()).collect();
        assert!(names.contains(&"list_dir"), "list_dir should match Seq(Entry(Name, Bytes)), got: {:?}", names);
    }

    #[test]
    fn test_extract_archive_unifies_with_zip_and_cbz() {
        let reg = build_fs_registry();

        // Zip archive
        let zip_target = TypeExpr::seq(TypeExpr::entry(
            TypeExpr::prim("Name"),
            TypeExpr::prim("Bytes"),
        ));
        let zip_matches = reg.ops_for_output_expr(&zip_target);
        let zip_names: Vec<&str> = zip_matches.iter().map(|m| m.op.name.as_str()).collect();
        assert!(zip_names.contains(&"extract_archive"), "extract_archive should match for Zip content");

        // CBZ archive (content is File(Image))
        let cbz_target = TypeExpr::seq(TypeExpr::entry(
            TypeExpr::prim("Name"),
            TypeExpr::file(TypeExpr::prim("Image")),
        ));
        let cbz_matches = reg.ops_for_output_expr(&cbz_target);
        let cbz_names: Vec<&str> = cbz_matches.iter().map(|m| m.op.name.as_str()).collect();
        assert!(cbz_names.contains(&"extract_archive"), "extract_archive should match for CBZ content");

        // Verify the input types are correctly specialized
        for m in &cbz_matches {
            if m.op.name == "extract_archive" {
                // Input should be File(Archive(File(Image), <var>))
                assert_eq!(m.concrete_inputs.len(), 1);
                let input = &m.concrete_inputs[0];
                match input {
                    TypeExpr::Constructor(name, args) if name == "File" => {
                        match &args[0] {
                            TypeExpr::Constructor(name2, args2) if name2 == "Archive" => {
                                assert_eq!(args2[0], TypeExpr::file(TypeExpr::prim("Image")));
                            }
                            other => panic!("expected Archive, got: {}", other),
                        }
                    }
                    other => panic!("expected File(Archive(...)), got: {}", other),
                }
            }
        }
    }

    #[test]
    fn test_concat_seq_associative_not_commutative() {
        let reg = build_fs_registry();
        let op = reg.get_poly("concat_seq").unwrap();
        assert!(op.properties.associative, "concat_seq should be associative");
        assert!(!op.properties.commutative, "concat_seq should NOT be commutative");
    }

    #[test]
    fn test_filter_is_idempotent() {
        let reg = build_fs_registry();
        let op = reg.get_poly("filter").unwrap();
        assert!(op.properties.idempotent, "filter should be idempotent");
    }

    #[test]
    fn test_sort_by_is_idempotent() {
        let reg = build_fs_registry();
        let op = reg.get_poly("sort_by").unwrap();
        assert!(op.properties.idempotent, "sort_by should be idempotent");
    }

    #[test]
    fn test_stat_is_idempotent() {
        let reg = build_fs_registry();
        let op = reg.get_poly("stat").unwrap();
        assert!(op.properties.idempotent, "stat should be idempotent");
    }

    #[test]
    fn test_pack_archive_signature() {
        let reg = build_fs_registry();
        let op = reg.get_poly("pack_archive").unwrap();
        assert_eq!(op.signature.inputs.len(), 2, "pack_archive needs 2 inputs");
        assert_eq!(op.signature.type_params.len(), 2, "pack_archive has 2 type params");
    }

    #[test]
    fn test_search_content_monomorphic() {
        let reg = build_fs_registry();
        let op = reg.get_poly("search_content").unwrap();
        assert!(op.signature.type_params.is_empty(), "search_content is monomorphic");
        assert_eq!(op.signature.inputs.len(), 2);
    }

    #[test]
    fn test_find_matching_idempotent() {
        let reg = build_fs_registry();
        let op = reg.get_poly("find_matching").unwrap();
        assert!(op.properties.idempotent, "find_matching should be idempotent");
    }

    #[test]
    fn test_concat_seq_has_identity() {
        let reg = build_fs_registry();
        let op = reg.get_poly("concat_seq").unwrap();
        assert_eq!(op.properties.identity, Some("[]".to_string()));
    }
}
