# rank_metrics

A grab-bag of metrics for evaluating ranking (e.g. NDCG).  I started this because while they are often well explained
on wikis, or other repos bundle them, I got tired of re-implementing and wanted a repo I knew I could always clone
with minimal license limitations (Apache V2).

Ben Hamner has a great metrics repo already at https://github.com/benhamner/Metrics, which is worth looking at
if you need more than ranking evaluation (although it also has ranking metrics).

Implemented:
 * DCG and NDCG @ position X (https://en.wikipedia.org/wiki/Discounted_cumulative_gain) in Python
 
In progress:
 * Cumulative NDCG and DCG @ position X


If you see something that could be better, I always appreciate a pull request :).
