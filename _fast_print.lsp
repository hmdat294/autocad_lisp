
(vl-load-com)

(defun c:FAST_PRINT
  (/ *error* acad doc lay plot
     blocks ent data obj mn mx
     x1 y1 x2 y2 w h p1 p2
     folder prefix outpdf tempdir pdfs pdfpath
     printer media ctb tool shell cmd exitcode
     idx count result oldecho oldplot
     oldfile page-name)

  ;; Khoi phuc bien he thong khi loi
  (defun *error* (msg)
    (if oldecho
      (setvar "CMDECHO" oldecho))
    (if oldplot
      (setvar "BACKGROUNDPLOT" oldplot))
    (if msg
      (prompt (strcat "\nLoi: " msg)))
    (princ)
  )

  (setq oldecho (getvar "CMDECHO")
        oldplot (getvar "BACKGROUNDPLOT"))

  (setvar "CMDECHO" 0)
  (setvar "BACKGROUNDPLOT" 0)

  (setq acad (vlax-get-acad-object)
        doc  (vla-get-ActiveDocument acad)
        lay  (vla-get-ActiveLayout doc)
        plot (vla-get-Plot doc))

  ;; Chon block lan luot theo thu tu trang
  (setq blocks nil)
  (prompt
    "\nClick lan luot cac BLOCK khung theo thu tu trang PDF.")
  (prompt "\nNhan Enter khi da chon xong.")

  (while
    (setq data
      (entsel "\nChon BLOCK khung (Enter de ket thuc): "))

    (setq ent (car data))

    (if
      (and ent
           (= "INSERT"
              (cdr (assoc 0 (entget ent)))))

      (if (member ent blocks)
        (prompt "\nBlock nay da duoc chon.")
        (progn
          (setq blocks (cons ent blocks))
          (prompt
            (strcat "\nDa chon trang "
                    (itoa (length blocks)))))
      )

      (prompt "\nDoi tuong khong phai BLOCK INSERT.")
    )
  )

  ;; Dao nguoc de giu dung thu tu click
  (setq blocks (reverse blocks))

  (if (null blocks)
    (prompt "\nChua chon block.")

    (progn

      ;; Chon file PDF dau ra
      (setq outpdf
        (getfiled
          "Chon noi luu PDF nhieu trang"
          (strcat (getvar "DWGPREFIX") "Banve.pdf")
          "pdf"
          1))

      (if outpdf
        (progn

          (setq folder (vl-filename-directory outpdf)
                prefix (vl-filename-base outpdf)
                printer "DWG To PDF.pc3"
                media "ISO_full_bleed_A3_(420.00_x_297.00_MM)"
                ctb "monochrome.ctb"
                count 0
                idx 0
                pdfs nil)

          ;; Tim PDF24 DocTool C:\Program Files (x86)\PDF24\pdf24-DocTool
          (setq tool
            "C:\Program Files (x86)\PDF24\pdf24-DocTool.exe")

          (if (not (findfile tool))
            (setq tool
              "C:\\Program Files (x86)\\PDF24\\pdf24-DocTool.exe"))

          (if (not (findfile tool))

            (prompt
              "\nKhong tim thay pdf24-DocTool.exe.")

            (progn

              ;; Thu muc PDF tam rieng cho lan chay nay
              (setq tempdir
                (strcat
                  (getvar "TEMPPREFIX")
                  "INNHANHA3_"
                  (itoa (getvar "MILLISECS"))
                  "\\"))

              (vl-mkdir tempdir)

              ;; Cau hinh may in
              (setq result
                (vl-catch-all-apply
                  '(lambda ()
                     (vla-put-ConfigName lay printer)
                     (vla-RefreshPlotDeviceInfo lay)
                     (vla-put-CanonicalMediaName lay media)
                     (vla-put-StyleSheet lay ctb)
                     (vla-put-PlotWithLineweights lay :vlax-true)
                     (vla-put-PlotWithPlotStyles lay :vlax-true)
                     (vla-put-CenterPlot lay :vlax-true)
                     (vla-put-UseStandardScale lay :vlax-true)
                     (vla-put-StandardScale lay 0)
                   )
                )
              )

              (if (vl-catch-all-error-p result)

                (prompt
                  (strcat "\nLoi cau hinh may in: "
                    (vl-catch-all-error-message result)))

                (progn

                  ;; In tung block thanh PDF tam
                  (foreach ent blocks

                    (setq idx (1+ idx)
                          obj (vlax-ename->vla-object ent)
                          mn nil
                          mx nil)

                    (setq result
                      (vl-catch-all-apply
                        'vla-GetBoundingBox
                        (list obj 'mn 'mx)))

                    (if (vl-catch-all-error-p result)

                      (prompt
                        (strcat "\nKhong doc duoc khung trang "
                          (itoa idx)))

                      (progn

                        (setq mn (vlax-safearray->list mn)
                              mx (vlax-safearray->list mx)
                              x1 (car mn)
                              y1 (cadr mn)
                              x2 (car mx)
                              y2 (cadr mx)
                              w (- x2 x1)
                              h (- y2 y1))

                        (if (and (> w 0.0) (> h 0.0))

                          (progn

                            ;; Tao toa do vung in
                            (setq p1
                              (vlax-make-safearray
                                vlax-vbDouble '(0 . 1)))
                            (setq p2
                              (vlax-make-safearray
                                vlax-vbDouble '(0 . 1)))

                            (vlax-safearray-put-element p1 0 x1)
                            (vlax-safearray-put-element p1 1 y1)
                            (vlax-safearray-put-element p2 0 x2)
                            (vlax-safearray-put-element p2 1 y2)

                            (setq pdfpath
                              (strcat tempdir
                                "Page_"
                                (if (< idx 10) "00"
                                  (if (< idx 100) "0" ""))
                                (itoa idx)
                                ".pdf"))

                            ;; Dat vung in va xuat PDF
                            (setq result
                              (vl-catch-all-apply
                                '(lambda ()
                                   (vla-SetWindowToPlot lay p1 p2)
                                   (vla-put-PlotType lay 4)
                                   (vla-put-PlotRotation
                                     lay (if (> w h) 0 1))
                                   (vla-PlotToFile plot pdfpath printer)
                                 )
                              )
                            )

                            (if
                              (and
                                (not (vl-catch-all-error-p result))
                                result
                                (findfile pdfpath))

                              (progn
                                (setq pdfs (cons pdfpath pdfs)
                                      count (1+ count))
                                (prompt
                                  (strcat "\nDa in trang "
                                    (itoa idx) ": " pdfpath))
                              )

                              (prompt
                                (strcat "\nIn trang "
                                  (itoa idx) " that bai."))
                            )
                          )

                          (prompt
                            (strcat "\nKhung trang "
                              (itoa idx) " co kich thuoc khong hop le."))
                        )
                      )
                    )
                  )

                  ;; Gop PDF theo thu tu trang
                  (if (= count (length blocks))

                    (progn
                      ;; pdfs dang dao nguoc, dao lai truoc khi gop
                      (setq pdfs (reverse pdfs))

                      (setq cmd
                        (strcat
                          "\""
                          tool
                          "\" -join -profile none -outputFile \""
                          outpdf
                          "\""))

                      (foreach pdfpath pdfs
                        (setq cmd
                          (strcat cmd " \"" pdfpath "\""))
                      )

                      (prompt "\nDang gop cac trang PDF...")

                      (setq shell
                        (vlax-create-object "WScript.Shell"))

                      ;; Cho PDF24 chay xong roi moi tiep tuc
                      (setq result
                        (vl-catch-all-apply
                          'vlax-invoke-method
                          (list shell 'Run
                            (strcat "cmd.exe /D /S /C \""
                              cmd "\"")
                            0
                            :vlax-true)
                        )
                      )

                      (if (vl-catch-all-error-p result)
                        (prompt
                          (strcat "\nKhong chay duoc PDF24: "
                            (vl-catch-all-error-message result)))

                        (if
                          (and
                            (= result 0)
                            (findfile outpdf))

                          (progn
                            (prompt
                              (strcat "\nGOP PDF THANH CONG: "
                                outpdf))

                            ;; Chi xoa file tam sau khi gop thanh cong
                            (foreach pdfpath pdfs
                              (if (findfile pdfpath)
                                (vl-file-delete pdfpath))
                            )
                            (vl-file-delete
                              (strcat tempdir "unused.tmp"))
                          )

                          (prompt
                            (strcat
                              "\nGop PDF that bai. Kiem tra PDF24."
                              "\nCac file PDF tam duoc giu tai: "
                              tempdir))
                        )
                      )

                      (if shell
                        (vlax-release-object shell))
                    )

                    (prompt
                      (strcat
                        "\nKhong du trang PDF de gop."
                        "\nDa in thanh cong "
                        (itoa count) "/"
                        (itoa (length blocks))
                        " trang. Cac file tam duoc giu lai."))
                  )
                )
              )
            )
          )
        )

        (prompt "\nDa huy chon noi luu PDF.")
      )
    )
  )

  (setvar "BACKGROUNDPLOT" oldplot)
  (setvar "CMDECHO" oldecho)
  (princ)
)
