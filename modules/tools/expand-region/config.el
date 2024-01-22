(use-package expand-region
  :vc (:fetcher github :repo magnars/expand-region.el)
  :defer t
  :bind (("C-=" . er/expand-region)
	 ("C--" . er/contract-region)))
