use cadmus::plan;
use cadmus::fs_types;

#[test]
fn trace_promote() {
    let yaml = r#"
plan: "Repack comics"
inputs:
  path: "/comics"
steps:
  - list_dir
  - find_matching:
      pattern: "*.cbz"
  - sort_by: name
  - extract_archive: each
  - flatten_seq
  - enumerate_entries
  - pack_archive:
      output: "combined.cbz"
"#;
    let def = plan::parse_plan(yaml).unwrap();
    let registry = fs_types::build_full_registry();
    match plan::compile_plan(&def, &registry) {
        Ok(compiled) => {
            println!("SUCCESS!");
            println!("input_type:  {}", compiled.input_type);
            println!("output_type: {}", compiled.output_type);
            for cs in &compiled.steps {
                println!("  step {}: {} :: {} -> {}", cs.index+1, cs.op, cs.input_type, cs.output_type);
            }

            // Generate Racket
            println!("\n=== RACKET ===");
            match cadmus::racket_executor::generate_racket_script(&compiled, &def, &registry) {
                Ok(script) => println!("{}", script),
                Err(e) => println!("CODEGEN ERROR: {:?}", e),
            }
        }
        Err(e) => {
            println!("COMPILE ERROR: {:?}", e);
        }
    }
}
