# Checkstyle Output Option for Lizard

## Requirements
- Add a new command-line option `--checkstyle` to Lizard.
- When specified, output results in Checkstyle-compatible XML format.
- Output should be valid for Jenkins Warnings Next Generation and other Checkstyle consumers.
- The option should be documented in `--help` and README.
- The output should be available via CLI and by inferring from output file extension (e.g., `.checkstyle.xml`).
- Add tests to verify:
  - CLI accepts `--checkstyle`.
  - Output is valid Checkstyle XML.
  - Output is accepted by Checkstyle consumers.
  - Help message includes the new option.

## Design Notes
- Follow the pattern of `xmloutput.py` and `csvoutput.py` in `lizard_ext/`.
- Register the new output in `lizard_ext/__init__.py`.
- Add a printer function (e.g., `print_checkstyle`).
- Integrate with CLI argument parsing in `lizard.py`.
- Add/extend tests in `test/` for CLI, output, and file output behaviors.
- Update documentation as needed. 