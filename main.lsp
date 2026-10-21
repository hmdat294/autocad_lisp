
(vl-load-com)

(setq folder (vl-filename-directory (findfile "main.lsp")))

(setq arr_file_name '("_acreage.lsp" "_fast_print.lsp" "_operation.lsp"))

(foreach f arr_file_name
  (setq path (strcat folder "\\src\\" f))
  (if (findfile path)
    (load path)
  )
)

(princ)