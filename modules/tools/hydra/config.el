(use-package hydra
  :vc t
  :defer t)

(defhydra hydra-vc (:color blue :hint nil)
  "
^File^               ^VC Directory^        ^Other^
^^^^^^^^-------------------------------------------------
_s_: VC Next Action   _D_: VC Dir           _l_: VC Print Log
_r_: VC Revert        _g_: VC Annotate      _i_: VC Register
_d_: VC Diff          _u_: VC Update        _+_ : VC Incremental Compile
_c_: VC Commit        _t_: VC Create Tag    _m_: VC Merge
_b_: VC Blame         _p_: VC Pull          _P_: VC Push
"
  ("s" vc-next-action)
  ("r" vc-revert)
  ("d" vc-diff)
  ("c" vc-dir)
  ("b" vc-blame)
  ("D" vc-dir)
  ("g" vc-annotate)
  ("u" vc-update)
  ("t" vc-create-tag)
  ("p" vc-pull)
  ("P" vc-push)
  ("l" vc-print-log)
  ("i" vc-register)
  ("+" vc-incremental-compile)
  ("m" vc-merge)
  ("q" nil "quit" :color red))

(global-set-key (kbd "C-c v") 'hydra-vc/body)

(defhydra hydra-registers (:color blue :hint nil)
  "
^Copy^            ^Insert^           ^Jump^            ^Number^         ^Frames^         ^Bookmarks^
^^^^^^^^------------------------------------------------------------------------------------------
_s_: Copy String  _i_: Insert Reg    _j_: Jump to Reg  _+_: Increment   _f_: Frames      _b_: Bookmark
_r_: Copy Rect    _w_: Window Conf   _n_: Number       _-_: Decrement   _l_: List Regs   _m_: Bookmark Jump
_p_: Point to Reg                  ^^                 ^^               ^^               _s_: Save Bookmark
"
  ("s" copy-to-register)
  ("r" copy-rectangle-to-register)
  ("p" point-to-register)
  ("i" insert-register)
  ("w" window-configuration-to-register)
  ("j" jump-to-register)
  ("n" number-to-register)
  ("+" increment-register)
  ("-" decrement-register)
  ("f" frame-configuration-to-register)
  ("l" list-registers)
  ("b" bookmark-set)
  ("m" bookmark-jump)
  ("s" bookmark-save)
  ("q" nil "quit" :color red))

(global-set-key (kbd "C-c r") 'hydra-registers/body)
