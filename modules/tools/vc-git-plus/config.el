(use-package vc-git-plus
  :load-path "~/.sandbox/vc-git-plus"
  :bind (("C-c ." . vc-git-init)
	 ("C-x v ." . vc-git-init)
	 ("C-x v /" . vc-git-branch-delete)
	 ))
