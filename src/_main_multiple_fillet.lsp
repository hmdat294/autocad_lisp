(vl-load-com)

(setq folder_mfl (vl-filename-directory (findfile "src\\_main_multiple_fillet.lsp")))

(setq arr_file_name_mfl nil)

(defun arr_insert_fillet_module (item) 
  (setq arr_file_name_mfl (append arr_file_name_mfl (list item)))
)

(arr_insert_fillet_module "MFL_Geometry.lsp")
(arr_insert_fillet_module "MFL_Pairing.lsp")
(arr_insert_fillet_module "MFL_Arc.lsp")
(arr_insert_fillet_module "MFL_Main.lsp")

(foreach f arr_file_name_mfl 
  (setq path (strcat folder_mfl "\\multiple_fillet\\" f))
  (if (findfile path) (load path))
)

(princ)