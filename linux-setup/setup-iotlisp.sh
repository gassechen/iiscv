#!/bin/bash
# setup-iotlisp.sh
# Configura usuario iotlispuser con Quicklisp, IISCV e imagen de desarrollo

set -e

USERNAME="iotlispuser"
IISCV_REPO="https://github.com/gassechen/iiscv.git"
DEV_IMAGE="/home/$USERNAME/dev.core"

echo "=== CONFIGURANDO USUARIO IOTLISPUSER ==="
echo ""

# 1. Crear usuario iotlispuser
echo "[1/6] Creando usuario..."
if ! id "$USERNAME" &>/dev/null; then
    useradd -m -s /bin/bash "$USERNAME"
    echo "$USERNAME:iotlisp" | chpasswd
    echo "Usuario $USERNAME creado"
else
    echo "Usuario $USERNAME ya existe"
fi

echo ""
echo "[2/6] Instalando Quicklisp..."
su - "$USERNAME" -c "curl -O https://beta.quicklisp.org/quicklisp.lisp"
su - "$USERNAME" -c "sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)'"

echo ""
echo "[3/6] Clonando IISCV..."
su - "$USERNAME" -c "mkdir -p quicklisp/local-projects"
su - "$USERNAME" -c "cd quicklisp/local-projects && git clone $IISCV_REPO iiscv"

echo ""
echo "[4/6] Creando imagen de desarrollo con IISCV..."
su - "$USERNAME" -c "sbcl --load quicklisp/setup.lisp --eval '(ql:quickload :iiscv)' --eval '(save-lisp-and-die \"$DEV_IMAGE\" :toplevel #\\'iiscv:iiscv-repl :executable t)' --no-userinit --no-sysinit"

echo ""
echo "[5/6] Configurando permisos de la imagen..."
chmod +x "$DEV_IMAGE"
chown "$USERNAME:$USERNAME" "$DEV_IMAGE"

echo ""
echo "[6/6] Configurando shell de login..."
if ! grep -q "$DEV_IMAGE" /etc/shells; then
    echo "$DEV_IMAGE" >> /etc/shells
fi
chsh -s "$DEV_IMAGE" "$USERNAME"

echo ""
echo "=== CONFIGURACIÓN COMPLETADA ==="
echo ""
echo "Para iniciar sesión:"
echo "  ssh $USERNAME@<ip-de-la-maquina>"
echo ""
echo "O localmente:"
echo "  su - $USERNAME"
echo ""
echo "Verás el prompt: IISCV-R>"
