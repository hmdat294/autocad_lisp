(vl-load-com)

(setq folder (vl-filename-directory (findfile "main.lsp")))

(setq arr_file_name nil)

(defun arr_insert (item) 
  (setq arr_file_name (append arr_file_name (list item)))
)

(arr_insert "_acreage.lsp")
(arr_insert "_fast_print.lsp")
(arr_insert "_operation.lsp")
(arr_insert "_main_multiple_fillet.lsp")

(foreach f arr_file_name 
  (setq path (strcat folder "\\src\\" f))
  (if (findfile path) (load path))
)

(princ)