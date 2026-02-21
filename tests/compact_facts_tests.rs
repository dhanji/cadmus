// tests/compact_facts_tests.rs — Round-trip validation for compact_properties format

use cadmus::fact_pack::{load_fact_pack, FactPack, FactPackIndex};
use std::path::PathBuf;

// ===========================================================================
// Property count validation — each fact pack has the expected number
// ===========================================================================

#[test]
fn test_racket_facts_property_count() {
    let path = PathBuf::from("data/packs/facts/racket_facts.yaml");
    let idx = load_fact_pack(&path).expect("should load racket_facts");
    assert_eq!(
        idx.pack.properties.len(),
        312,
        "racket_facts should have exactly 312 properties"
    );
}

#[test]
fn test_macos_cli_facts_property_count() {
    let path = PathBuf::from("data/packs/facts/macos_cli_facts.yaml");
    let idx = load_fact_pack(&path).expect("should load macos_cli_facts");
    assert_eq!(
        idx.pack.properties.len(),
        461,
        "macos_cli_facts should have exactly 461 properties"
    );
}

#[test]
fn test_putin_stalin_property_count() {
    let path = PathBuf::from("data/packs/facts/putin_stalin.yaml");
    let idx = load_fact_pack(&path).expect("should load putin_stalin");
    assert_eq!(
        idx.pack.properties.len(),
        14,
        "putin_stalin should have exactly 14 properties"
    );
}

// ===========================================================================
// Ordinal + note preservation (putin_stalin)
// ===========================================================================

#[test]
fn test_putin_stalin_ordinals_preserved() {
    let path = PathBuf::from("data/packs/facts/putin_stalin.yaml");
    let idx = load_fact_pack(&path).unwrap();

    // Every property in putin_stalin has an ordinal
    for p in &idx.pack.properties {
        assert!(
            p.ordinal.is_some(),
            "property {}.{}.{} should have ordinal",
            p.entity, p.axis, p.key
        );
    }

    // Check specific values
    let stalin_coercion = idx
        .pack
        .properties
        .iter()
        .find(|p| p.entity == "stalin" && p.key == "repression_scale")
        .expect("stalin repression_scale");
    assert_eq!(stalin_coercion.value, "mass");
    assert_eq!(stalin_coercion.ordinal, Some(5));

    let putin_coercion = idx
        .pack
        .properties
        .iter()
        .find(|p| p.entity == "putin" && p.key == "repression_scale")
        .expect("putin repression_scale");
    assert_eq!(putin_coercion.value, "targeted");
    assert_eq!(putin_coercion.ordinal, Some(2));
}

#[test]
fn test_putin_stalin_notes_preserved() {
    let path = PathBuf::from("data/packs/facts/putin_stalin.yaml");
    let idx = load_fact_pack(&path).unwrap();

    // Every property in putin_stalin has a note
    for p in &idx.pack.properties {
        assert!(
            p.note.is_some(),
            "property {}.{}.{} should have note",
            p.entity, p.axis, p.key
        );
    }

    // Check a specific note
    let stalin_ideology = idx
        .pack
        .properties
        .iter()
        .find(|p| p.entity == "stalin" && p.key == "ideological_coherence")
        .expect("stalin ideological_coherence");
    assert!(
        stalin_ideology.note.as_ref().unwrap().contains("Marxism-Leninism"),
        "note should mention Marxism-Leninism"
    );
}

// ===========================================================================
// FactPackIndex builds correctly from compact format
// ===========================================================================

#[test]
fn test_racket_facts_index_queries() {
    let path = PathBuf::from("data/packs/facts/racket_facts.yaml");
    let idx = load_fact_pack(&path).unwrap();

    // Check property index: op_add should have racket_symbol
    let add_symbol = idx.properties_by_axis_key_entity.get(&(
        "type_signature".into(),
        "racket_symbol".into(),
        "op_add".into(),
    ));
    assert!(add_symbol.is_some(), "op_add should have racket_symbol");
    let p = &idx.pack.properties[*add_symbol.unwrap()];
    assert_eq!(p.value, "+");

    // Check axis-key index: type_symmetry_class should have entries for all entities
    let symmetry_entries = idx
        .properties_by_axis_key
        .get(&("type_symmetry".into(), "type_symmetry_class".into()));
    assert!(symmetry_entries.is_some());
    assert!(
        symmetry_entries.unwrap().len() >= 10,
        "should have many type_symmetry_class entries"
    );
}

#[test]
fn test_macos_cli_facts_index_queries() {
    let path = PathBuf::from("data/packs/facts/macos_cli_facts.yaml");
    let idx = load_fact_pack(&path).unwrap();

    // Check git submodes
    let git_clone = idx.properties_by_axis_key_entity.get(&(
        "shell_submodes".into(),
        "submode_clone".into(),
        "cli_git".into(),
    ));
    assert!(git_clone.is_some(), "cli_git should have submode_clone");
    let p = &idx.pack.properties[*git_clone.unwrap()];
    assert_eq!(p.value, "clone");

    // Check base_command for ls
    let ls_cmd = idx.properties_by_axis_key_entity.get(&(
        "shell_type_signature".into(),
        "base_command".into(),
        "cli_ls".into(),
    ));
    assert!(ls_cmd.is_some(), "cli_ls should have base_command");
    let p = &idx.pack.properties[*ls_cmd.unwrap()];
    assert_eq!(p.value, "ls");
}

#[test]
fn test_putin_stalin_index_queries() {
    let path = PathBuf::from("data/packs/facts/putin_stalin.yaml");
    let idx = load_fact_pack(&path).unwrap();

    // Check ordinal comparison via index
    let putin_coercion = idx.properties_by_axis_key_entity.get(&(
        "coercion".into(),
        "repression_scale".into(),
        "putin".into(),
    ));
    let stalin_coercion = idx.properties_by_axis_key_entity.get(&(
        "coercion".into(),
        "repression_scale".into(),
        "stalin".into(),
    ));
    assert!(putin_coercion.is_some());
    assert!(stalin_coercion.is_some());

    let p_putin = &idx.pack.properties[*putin_coercion.unwrap()];
    let p_stalin = &idx.pack.properties[*stalin_coercion.unwrap()];
    assert!(
        p_stalin.ordinal.unwrap() > p_putin.ordinal.unwrap(),
        "stalin should have higher ordinal than putin for repression"
    );
}

// ===========================================================================
// Keyword properties (racket_facts — variable count per entity)
// ===========================================================================

#[test]
fn test_racket_facts_keyword_properties() {
    let path = PathBuf::from("data/packs/facts/racket_facts.yaml");
    let idx = load_fact_pack(&path).unwrap();

    // op_add has 6 keyword properties
    let add_keywords: Vec<_> = idx
        .pack
        .properties
        .iter()
        .filter(|p| p.entity == "op_add" && p.key.starts_with("keyword_"))
        .collect();
    assert_eq!(
        add_keywords.len(),
        6,
        "op_add should have 6 keyword properties"
    );

    // Check specific keyword values
    let kw_plus = add_keywords.iter().find(|p| p.key == "keyword_plus");
    assert!(kw_plus.is_some());
    assert_eq!(kw_plus.unwrap().value, "plus");
}

#[test]
fn test_macos_cli_git_submodes() {
    let path = PathBuf::from("data/packs/facts/macos_cli_facts.yaml");
    let idx = load_fact_pack(&path).unwrap();

    // cli_git should have many submodes
    let git_submodes: Vec<_> = idx
        .pack
        .properties
        .iter()
        .filter(|p| p.entity == "cli_git" && p.key.starts_with("submode_"))
        .collect();
    assert!(
        git_submodes.len() >= 15,
        "cli_git should have at least 15 submodes, got {}",
        git_submodes.len()
    );
}

// ===========================================================================
// Empty / missing compact_properties
// ===========================================================================

#[test]
fn test_empty_compact_properties() {
    let yaml = r#"
entities:
  - id: a
    name: A
    description: test
axes:
  - id: x
    name: X
    description: test
    sub_axes: []
claims: []
evidence: []
compact_properties: {}
relations: []
uncertainties: []
"#;
    let pack: FactPack = serde_yaml::from_str(yaml).unwrap();
    assert!(pack.properties.is_empty());
}

#[test]
fn test_missing_compact_properties() {
    let yaml = r#"
entities:
  - id: a
    name: A
    description: test
axes:
  - id: x
    name: X
    description: test
    sub_axes: []
claims: []
evidence: []
relations: []
uncertainties: []
"#;
    let pack: FactPack = serde_yaml::from_str(yaml).unwrap();
    assert!(pack.properties.is_empty());
}

// ===========================================================================
// Error cases
// ===========================================================================

#[test]
fn test_malformed_compact_value_type() {
    // A list where a string/map is expected
    let yaml = r#"
entities: []
axes: []
claims: []
evidence: []
compact_properties:
  entity_a:
    axis_x:
      key_1:
        - not
        - a
        - valid
        - value
"#;
    let result: std::result::Result<FactPack, _> = serde_yaml::from_str(yaml);
    assert!(result.is_err(), "should fail on list value");
}

#[test]
fn test_compact_simple_and_extended_values() {
    let yaml = r#"
entities:
  - id: a
    name: A
    description: test
axes:
  - id: x
    name: X
    description: test
    sub_axes: []
claims: []
evidence: []
compact_properties:
  a:
    x:
      simple_key: hello
      extended_key:
        value: world
        ordinal: 42
        note: "a note"
relations: []
uncertainties: []
"#;
    let pack: FactPack = serde_yaml::from_str(yaml).unwrap();
    assert_eq!(pack.properties.len(), 2);

    let simple = pack.properties.iter().find(|p| p.key == "extended_key").unwrap();
    assert_eq!(simple.value, "world");
    assert_eq!(simple.ordinal, Some(42));
    assert_eq!(simple.note.as_deref(), Some("a note"));

    let plain = pack.properties.iter().find(|p| p.key == "simple_key").unwrap();
    assert_eq!(plain.value, "hello");
    assert!(plain.ordinal.is_none());
    assert!(plain.note.is_none());
}

#[test]
fn test_compact_extended_value_without_note() {
    let yaml = r#"
entities: []
axes: []
claims: []
evidence: []
compact_properties:
  ent:
    ax:
      k:
        value: v
        ordinal: 7
"#;
    let pack: FactPack = serde_yaml::from_str(yaml).unwrap();
    assert_eq!(pack.properties.len(), 1);
    let p = &pack.properties[0];
    assert_eq!(p.entity, "ent");
    assert_eq!(p.axis, "ax");
    assert_eq!(p.key, "k");
    assert_eq!(p.value, "v");
    assert_eq!(p.ordinal, Some(7));
    assert!(p.note.is_none());
}

// ===========================================================================
// Cross-entity property grouping
// ===========================================================================

#[test]
fn test_compact_multiple_entities_same_axis() {
    let yaml = r#"
entities:
  - id: a
    name: A
    description: test
  - id: b
    name: B
    description: test
axes:
  - id: x
    name: X
    description: test
    sub_axes: []
claims: []
evidence: []
compact_properties:
  a:
    x:
      level: high
  b:
    x:
      level: low
relations: []
uncertainties: []
"#;
    let pack: FactPack = serde_yaml::from_str(yaml).unwrap();
    let idx = FactPackIndex::build(pack);

    // Both entities should be indexed
    let a_level = idx.properties_by_axis_key_entity.get(&(
        "x".into(),
        "level".into(),
        "a".into(),
    ));
    let b_level = idx.properties_by_axis_key_entity.get(&(
        "x".into(),
        "level".into(),
        "b".into(),
    ));
    assert!(a_level.is_some());
    assert!(b_level.is_some());

    // axis-key index should have both
    let levels = idx.properties_by_axis_key.get(&("x".into(), "level".into()));
    assert_eq!(levels.unwrap().len(), 2);
}
