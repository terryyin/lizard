# FabioMalpezzi review cohort and story mapping

The original 19-PR execution-start set is preserved verbatim in
[frozen-review-set.json](frozen-review-set.json). The owner requested three
bounded stories: finish/wrap the current eight, queue the remaining two batches,
and run those later. Every story owns full cycles, not horizontal QA phases.

| Story | PRs | State and value |
| --- | --- | --- |
|SEED-010#review-fabio-malpezzi-prs|527,528,531,533,534,532,535,536|Completed eight-disposition review scope; original proof retained. PR536's later SEED-016 correction is merged and acknowledged below.|
|[SEED-010#review-structural-js-prs](../../../.planning/seeds/SEED-010-fabio-malpezzi-pr-reviews.md#review-structural-js-prs)|537–543|Queued; refined with realigned seven-slice plan. Complete function/parameter/boundary review cycles.|
|[SEED-010#review-lexical-js-prs](../../../.planning/seeds/SEED-010-fabio-malpezzi-pr-reviews.md#review-lexical-js-prs)|544–547|Completed four-disposition lexical/condition review scope (#544/#546/#547 merged; #545 left unmerged on performance).|

The first identity remains stable; its scope is the human-authorized first eight
reviews. Nothing is marked delivered merely because split.
The lexical batch is disposition-complete; the structural sibling remains queued.
Each delivers independently of a sibling; the actual last cohort finisher
reconciles all 19 and combined evidence.

## Every original slice is mapped once

| Original slice | PR | Frozen head | Owner / mapped slice | Current disposition |
| --- | --- | --- | --- | --- |
|1|[#527](https://github.com/terryyin/lizard/pull/527)|`a529750edc95d44bc71fbcd88d567bbbef865ae5`|SEED-010#review-fabio-malpezzi-prs / 1|Merged e871ae4; thanked|
|2|[#528](https://github.com/terryyin/lizard/pull/528)|`317d5c83647b849f8b683c150a6039e47ce5ba2a`|SEED-010#review-fabio-malpezzi-prs / 2|Merged `6408864eeab300296617902c0c8980a3f11f2a39` with acknowledgement|
|3|[#531](https://github.com/terryyin/lizard/pull/531)|`47aa35735cccba962e507edb179eae1154ab6ad2`|SEED-010#review-fabio-malpezzi-prs / 3|Deferred SEED-013#arrow-object-boundaries|
|4|[#533](https://github.com/terryyin/lizard/pull/533)|`15423852c04aed302f684395fe9522251d88a2f2`|SEED-010#review-fabio-malpezzi-prs / 4|Deferred SEED-014#multiline-arrow-boundaries|
|5|[#534](https://github.com/terryyin/lizard/pull/534)|`f4074ae860c7999cc37dd081a6f7ebfa777adc83`|SEED-010#review-fabio-malpezzi-prs / 5|Corrected under SEED-015#ternary-arrow-boundaries; merged `48de7f2` and [acknowledged](https://github.com/terryyin/lizard/pull/534#issuecomment-6451652906); [qualified proof](https://github.com/terryyin/lizard/blob/0bfc7983aa33057971a81ead65349e8ad87b5231/docs/reviews/fabio-malpezzi-20261027/pr534/root-performance.md)|
|6|[#532](https://github.com/terryyin/lizard/pull/532)|`d0713bf5b4dcf4bf24453156a3756688627abbc0`|SEED-010#review-fabio-malpezzi-prs / 6|Merged 9bc4456; thanked|
|7|[#535](https://github.com/terryyin/lizard/pull/535)|`171079e625966ac9c17aee29a5d74256befd85ae`|SEED-010#review-fabio-malpezzi-prs / 7|Merged d7be8b9; thanked|
|8|[#536](https://github.com/terryyin/lizard/pull/536)|`92fff8a1621ab82b1ce296d25ac4eb2cdd892c8f`|SEED-010#review-fabio-malpezzi-prs / 8|Corrected under SEED-016#arrow-parameter-bindings; merged `1bc7cbd` and [acknowledged](https://github.com/terryyin/lizard/pull/536#issuecomment-6447350182); [qualified proof](https://github.com/terryyin/lizard/blob/4bfd49bc7eb7cd136f6adb3bfad3ab93f9a1631b/docs/reviews/fabio-malpezzi-20261027/pr536/slice5-proof.md)|
|9|[#537](https://github.com/terryyin/lizard/pull/537)|`c89470c442d28195bebba45597910c7a481a5043`|SEED-010#review-structural-js-prs / 1|Pending full review cycle|
|10|[#538](https://github.com/terryyin/lizard/pull/538)|`c869db90cf2ab6ba93b5c368b71f66e3eaaa6014`|SEED-010#review-structural-js-prs / 2|Pending full review cycle|
|11|[#539](https://github.com/terryyin/lizard/pull/539)|`daa7d2068ad1bbaaa201b9685aa5d21582ec0015`|SEED-010#review-structural-js-prs / 3|Pending full review cycle|
|12|[#540](https://github.com/terryyin/lizard/pull/540)|`d225a6d60af856ac155b882d1a3770b7d66457ef`|SEED-010#review-structural-js-prs / 4|Pending full review cycle|
|13|[#541](https://github.com/terryyin/lizard/pull/541)|`644480ce9f46892fae258696583a6c18649c05dc`|SEED-010#review-structural-js-prs / 5|Pending full review cycle|
|14|[#542](https://github.com/terryyin/lizard/pull/542)|`ddf075a0cdee4079e584919d818c28b6468a84b0`|SEED-010#review-structural-js-prs / 6|Pending full review cycle|
|15|[#543](https://github.com/terryyin/lizard/pull/543)|`f6a329b488bb1a6e5aa9af43f0cf3e5e761d8353`|SEED-010#review-structural-js-prs / 7|Pending full review cycle|
|16|[#544](https://github.com/terryyin/lizard/pull/544)|`3ac7e4eaf61e898836fded88ab715b37f9fedbbf`|SEED-010#review-lexical-js-prs / 1|Merged `35b454b` at corrected head `77902f7`; [acknowledged](https://github.com/terryyin/lizard/pull/544#issuecomment-6456484491); [review record](pr544/README.md)|
|17|[#545](https://github.com/terryyin/lizard/pull/545)|`baa95a4df8278421bb3d3a6487516030cc03a979`|SEED-010#review-lexical-js-prs / 2|Left unmerged — real defect; adapted candidate fails incremental names/counts &lt;3% upper CI after quiet longer retry; [review record](pr545/README.md); follow-up [SEED-019#template-interpolation-conditions](../../../.planning/seeds/SEED-019-template-interpolation-conditions.md#template-interpolation-conditions)|
|18|[#546](https://github.com/terryyin/lizard/pull/546)|`a671632141127fca9b2785fdbb0c675d45ae9042`|SEED-010#review-lexical-js-prs / 3|Merged `559b8c8` at corrected head `395ae04`; [acknowledged](https://github.com/terryyin/lizard/pull/546#issuecomment-6461965881); [review record](pr546/README.md)|
|19|[#547](https://github.com/terryyin/lizard/pull/547)|`2766557096daac3f8e55657e9c48ba685157635d`|SEED-010#review-lexical-js-prs / 4|Merged `8567a4c` at corrected head `bf75e50`; [acknowledged](https://github.com/terryyin/lizard/pull/547#issuecomment-6463154289); [review record](pr547/README.md)|

## Proof and priorities

First-eight proof is retained in the PR discussions and per-PR directories here.
The common contract preserves strict <3%, deferral above 15 minutes, complete quality/CI
and post-merge thanks. No correction or acceptance phase spills to another batch.
New review batches are queued after existing known correction stories because
the owner said they are not urgent; existing relative priority is preserved.
Structural work precedes lexical work for broader function-metric impact, not
because of an unproved dependency. Future refinement reuses the mapped plans.

The first batch's completed source, plan, obligations and proof are recoverable
at [625971ab](https://github.com/terryyin/lizard/tree/625971ab2c2d61249d1038f63e3dde25da166d84/.planning/slice-plans/004-fabio-malpezzi-pr-reviews).
PR536's original deferral is retained in its historical review; its technical
correction now has a qualified merge and acknowledgement. The structural and
lexical review batch is disposition-complete above; the structural batch remains queued.
