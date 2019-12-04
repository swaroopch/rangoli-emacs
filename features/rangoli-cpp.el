;; rangoli-cpp.el --- C++ configuration -*- lexical-binding: t; -*-

(straight-use-package 'cmake-mode)

(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

(provide 'rangoli-cpp)
;; rangoli-cpp.el ends here
