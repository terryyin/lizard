# PR #448 disposition (SEED-049)

- Head: `7a788b2d1a4e57b6a532ef3b31011c2daf1adcd1` on `Ollec:#441_trait_with_multiple_functions`
- Base: `dd1f9fc51733ded210024a9760d6821997ec97d5`
- `maintainerCanModify`: true; merge state CONFLICTING/DIRTY
- PR adjusts `_function_body` end condition to treat `in_trait` like `in_class`
  and adds `test_trait_with_multiple_functions` (issue #441)
- Trunk already ends each function when brace depth returns to
  `function_brace_level` in `php_states.py`
  (`69b29cdf9a241baadd129cc0990ac9efef24c783` for #498), with regression
  `test_each_trait_method_ends_at_its_own_brace`
- Trunk reproduction: trait and class both report `A::foo` / `A::bar` at
  nloc 3 with correct line spans; PHP suite green (30 passed focused)
- Performance: unaffected (no candidate merge; trunk behavior already correct)
- Packet: `.planning/evidence/pr-448/terryyin--lizard--pr448/4d3a2b5d200a0b8b270e362c1a3c7bbaee2fe87664f3f237a504872d50d59824`
- Disposition: **closed unmerged** — already fixed / superseded

Judgment: https://github.com/terryyin/lizard/pull/448#issuecomment-6702369846
