#!/usr/bin/env python3
"""Validate all algorithm plans in data/plans/algorithms/.

Checks:
  1. Every .yaml file has a '# prompt:' header on line 1
  2. Every .yaml file parses as valid YAML
  3. Every plan has a unique top-level key (plan name)
  4. No category directory is empty
  5. Total plan count is between 95 and 115
"""

import glob
import sys
import yaml

def main():
    plans = sorted(glob.glob("data/plans/algorithms/**/*.yaml", recursive=True))
    errors = []
    names = {}
    
    # Check total count
    count = len(plans)
    print(f"Total plans: {count}")
    if count < 95 or count > 115:
        errors.append(f"Plan count {count} outside expected range [95, 115]")
    
    # Check each plan
    for path in plans:
        with open(path) as f:
            lines = f.readlines()
        
        # Check prompt header
        if not lines or not lines[0].startswith("# prompt:"):
            errors.append(f"{path}: missing '# prompt:' header on line 1")
        
        # Check valid YAML
        try:
            with open(path) as f:
                content = f.read()
            data = yaml.safe_load(content)
        except Exception as e:
            errors.append(f"{path}: YAML parse error: {e}")
            continue
        
        # Check plan name uniqueness
        if isinstance(data, dict):
            for key in data:
                if key in names:
                    errors.append(f"{path}: duplicate plan name '{key}' (also in {names[key]})")
                names[key] = path
    
    # Check no empty categories
    categories = sorted(glob.glob("data/plans/algorithms/*/"))
    for cat in categories:
        cat_plans = glob.glob(f"{cat}*.yaml")
        if not cat_plans:
            errors.append(f"{cat}: empty category directory")
    
    # Report
    print(f"Categories: {len(categories)}")
    print(f"Unique plan names: {len(names)}")
    
    if errors:
        print(f"\n{len(errors)} ERROR(S):")
        for e in errors:
            print(f"  ✗ {e}")
        sys.exit(1)
    else:
        print(f"\n✓ All {count} plans valid. {len(categories)} categories, {len(names)} unique names.")
        sys.exit(0)

if __name__ == "__main__":
    main()
