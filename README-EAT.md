# Soporte para Eat Terminal en emacs-claude-code

Este documento describe la implementación de soporte para `eat` terminal como alternativa a `vterm` en emacs-claude-code.

## Archivos Implementados

### 1. `src/ecc-eat-utils.el`
Utilidades principales para integración con eat:
- `--ecc-eat-send-command`: Envía comandos al terminal eat
- `--ecc-eat-send-string`: Envía strings sin return
- `--ecc-eat-optimize-scrolling`: Optimizaciones de scrolling basadas en claudemacs
- `--ecc-eat-process-live-p`: Detecta si el proceso eat está activo

### 2. `src/ecc-eat-yank-as-file.el`
Funcionalidad de yank-as-file adaptada para eat:
- `emacs-claude-code-eat-yank-as-file`: Función principal
- `ecc-eat-yank-as-file`: Alias para compatibilidad
- Soporte para localhost y servidores remotos

### 3. Modificaciones existentes
- `src/ecc-auto-periodical.el`: Añadido soporte para eat-mode

## Optimizaciones de Scrolling

Basado en las técnicas de claudemacs para resolver problemas de scrolling en eat:

```elisp
;; Configuración de scroll conservativo
(setq-local scroll-conservatively 10000)
(setq-local scroll-margin 0)
(setq-local maximum-scroll-margin 0)

;; Deshabilitar ajustes automáticos
(setq-local auto-window-vscroll nil)
(setq-local eat-enable-blinking-text nil)

;; Reemplazar caracteres problemáticos
(let ((display-table (make-display-table)))
  (aset display-table #x23fa [?✽])  ; Reemplaza ⏺ con ✽
  (setq-local buffer-display-table display-table))
```

## Diferencias con vterm

| Aspecto | vterm | eat |
|---------|-------|-----|
| **Instalación** | Requiere compilación nativa | Puro Elisp |
| **Rendimiento** | Más rápido | Puede ser más lento |
| **Scrolling** | Más estable | Requiere optimizaciones |
| **Integración** | `vterm-send-string` | `eat-term-send-string` |
| **Detección proceso** | `vterm--process` | `eat-term-parameter` |

## Uso

### Configuración básica
```elisp
(require 'ecc-eat-utils)
(require 'ecc-eat-yank-as-file)
```

### Comandos principales
- `M-x emacs-claude-code-eat-yank-as-file`: Crear archivo temporal desde kill-ring
- `C-c C-y`: Keybinding sugerido (comentado en el código)

### Detección automática
El sistema detecta automáticamente si estás en `eat-mode` y usa las funciones apropiadas.

## Tests

Implementados tests completos en:
- `tests/test-ecc-eat-utils.el`
- `tests/test-ecc-eat-yank-as-file.el`

Ejecutar tests:
```bash
./run_tests.sh
```

## Compatibilidad

La implementación es completamente compatible con la versión vterm existente:
- No rompe funcionalidad existente
- Detección automática de terminal type
- Fallback graceful si eat no está disponible

## Problemas Conocidos

1. **Scrolling**: Eat puede tener problemas de scroll, especialmente con outputs largos
2. **Rendimiento**: Puede ser más lento que vterm con grandes cantidades de texto
3. **Caracteres especiales**: Algunos caracteres parpadeantes pueden causar problemas de display

Las optimizaciones implementadas mitigan estos problemas en gran medida.