# Multi-Agent Claude Management

Esta funcionalidad te permite ejecutar múltiples agentes Claude simultáneamente en diferentes terminales EAT, perfecta para trabajar en múltiples proyectos o tareas de desarrollo.

## 🚀 Funciones Principales

### Opción 4: Script de Automatización General

#### `ecc-multi-agent-setup-general`
Configura 3 agentes Claude en layout automático:
- **claude-main**: Agente principal 
- **claude-secondary**: Agente secundario
- **claude-project**: Agente para proyectos específicos

**Uso:**
```elisp
M-x ecc-multi-agent-setup-general
;; o con keybinding:
C-c e m g
```

### Configuración Recomendada para Tu Caso

#### `ecc-multi-agent-setup-ecc-tracker`
Configura 2 agentes específicos para tu workflow:
- **claude-ecc**: En `~/.config/doom/lisp/emacs-claude-code`
- **claude-tracker**: En `~/repo/tracker`

**Uso:**
```elisp
M-x ecc-multi-agent-setup-ecc-tracker
;; o con keybinding:
C-c e m e
```

## 🎛️ Funciones Adicionales

### Gestión Individual de Agentes

#### `ecc-multi-agent-create-agent`
Crea un agente individual con nombre y directorio personalizado.
```elisp
M-x ecc-multi-agent-create-agent
;; Keybinding: C-c e m n
```

#### `ecc-multi-agent-list-agents`
Lista todos los agentes Claude activos.
```elisp
M-x ecc-multi-agent-list-agents  
;; Keybinding: C-c e m l
```

#### `ecc-multi-agent-switch-to-agent`
Cambia rápidamente entre agentes activos.
```elisp
M-x ecc-multi-agent-switch-to-agent
;; Keybinding: C-c e m s
```

### Gestión Grupal

#### `ecc-multi-agent-send-to-all`
Envía un comando a todos los agentes activos simultáneamente.
```elisp
M-x ecc-multi-agent-send-to-all
;; Keybinding: C-c e m a
```

#### `ecc-multi-agent-kill-agent`
Termina un agente específico.
```elisp
M-x ecc-multi-agent-kill-agent
;; Keybinding: C-c e m k
```

#### `ecc-multi-agent-kill-all-agents`
Termina todos los agentes Claude activos.
```elisp
M-x ecc-multi-agent-kill-all-agents
;; Keybinding: C-c e m K
```

### Configuración Personalizada

#### `ecc-multi-agent-setup-custom-projects`
Te permite configurar agentes para proyectos personalizados, especificando directorios manualmente.
```elisp
M-x ecc-multi-agent-setup-custom-projects
;; Keybinding: C-c e m c
```

## 🎮 Menú Interactivo

#### `ecc-multi-agent-menu`
Muestra un menú interactivo con todas las opciones disponibles.
```elisp
M-x ecc-multi-agent-menu
;; Keybinding: C-c e m m
```

## ⌨️ Tabla de Keybindings

| Keybinding | Función | Descripción |
|------------|---------|-------------|
| `C-c e m m` | `ecc-multi-agent-menu` | Menú interactivo |
| `C-c e m g` | `ecc-multi-agent-setup-general` | 3 agentes generales |
| `C-c e m e` | `ecc-multi-agent-setup-ecc-tracker` | ECC + Tracker |
| `C-c e m c` | `ecc-multi-agent-setup-custom-projects` | Proyectos personalizados |
| `C-c e m n` | `ecc-multi-agent-create-agent` | Crear agente individual |
| `C-c e m l` | `ecc-multi-agent-list-agents` | Listar agentes |
| `C-c e m s` | `ecc-multi-agent-switch-to-agent` | Cambiar de agente |
| `C-c e m a` | `ecc-multi-agent-send-to-all` | Comando a todos |
| `C-c e m k` | `ecc-multi-agent-kill-agent` | Terminar agente |
| `C-c e m K` | `ecc-multi-agent-kill-all-agents` | Terminar todos |

## 🔧 Configuración

### Variables Personalizables

```elisp
;; Agentes por defecto
(setq ecc-multi-agent-default-agents
      '(("claude-main" . "~/")
        ("claude-secondary" . "~/projects")
        ("claude-work" . "~/work")))

;; Layout de ventanas
(setq ecc-multi-agent-window-layout 'vertical) ; 'horizontal, 'grid, 'tabs

;; Auto-iniciar claude-code
(setq ecc-multi-agent-auto-start-claude t)
```

## 📝 Ejemplos de Uso

### Caso 1: Desarrollo General
```elisp
;; Configura 3 agentes para desarrollo general
M-x ecc-multi-agent-setup-general

;; Envía comando a todos
C-c e m a
;; Escribe: git status

;; Cambia entre agentes
C-c e m s
```

### Caso 2: Tu Workflow Específico (ECC + Tracker)
```elisp
;; Configura agentes específicos para tu caso
M-x ecc-multi-agent-setup-ecc-tracker

;; Los agentes se crean automáticamente:
;; - claude-ecc: en ~/.config/doom/lisp/emacs-claude-code
;; - claude-tracker: en ~/repo/tracker

;; Usa C-x b para cambiar entre buffers:
C-x b *claude-ecc* RET
C-x b *claude-tracker* RET
```

### Caso 3: Proyectos Personalizados
```elisp
;; Configura agentes para proyectos específicos
M-x ecc-multi-agent-setup-custom-projects

;; Te pedirá:
;; - Directorio del proyecto 1: ~/mi-proyecto
;; - Nombre del agente: claude-mi-proyecto
;; - Directorio del proyecto 2: ~/otro-proyecto  
;; - Nombre del agente: claude-otro
;; (presiona ENTER en directorio vacío para terminar)
```

## 🔄 Navegación Entre Agentes

Una vez que tienes múltiples agentes corriendo:

1. **Cambio rápido**: `C-c e m s` → Selecciona de lista
2. **Buffer manual**: `C-x b *claude-nombre* RET`
3. **Ventana siguiente**: `C-x o` (si están en split)
4. **Lista de agentes**: `C-c e m l`

## 🛠️ Solución de Problemas

### Problema: Agente no responde
```elisp
;; Reinicia el agente específico
C-c e m k → Selecciona agente problemático
C-c e m n → Crea nuevo agente con mismo nombre
```

### Problema: Muchos agentes abiertos
```elisp
;; Limpia todos los agentes
C-c e m K
;; Confirma con 'y'
```

### Problema: Directorio incorrecto
```elisp
;; En el buffer del agente, cambia directorio manualmente:
cd /path/to/correct/directory
```

## ✨ Tips y Trucos

1. **Nombres descriptivos**: Usa nombres como `claude-frontend`, `claude-backend`, `claude-testing`
2. **Workflow consistency**: Mantén siempre la misma estructura de agentes para proyectos similares
3. **Comandos globales**: Usa `C-c e m a` para comandos que quieres ejecutar en todos los agentes (como `git status`)
4. **Quick access**: Memoriza `C-c e m e` para tu workflow específico ECC+Tracker

¡Disfruta trabajando con múltiples agentes Claude! 🚀