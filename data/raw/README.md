Place your downloaded `credit_risk_dataset.csv` here. Raw datasets are ignored by Git.

Suggested source from the original discussion: [Credit Risk Dataset by laotse on Kaggle](https://www.kaggle.com/datasets/laotse/credit-risk-dataset).
Download it yourself after reviewing the current dataset terms. This repository does not redistribute it or assume verified Indonesian provenance, currency, or a defined default observation window.

Required columns: `person_income`, `loan_amnt`, `person_emp_length`, `cb_person_default_on_file`, `loan_status`.
The first, second, third and fifth must parse as numeric. `loan_status` is 0/1 (missing allowed); previous default is Y/N.
Missing or invalid financial inputs go to manual review, unless consent is absent (no referral).

Without a CSV argument, the pipeline generates 10,000 reproducible demonstration customers. It never silently substitutes demo data for an unreadable external file.
