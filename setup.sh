#!/bin/bash

# 既存ファイルがあればバックアップ
if [ -f ~/.emacs ] && [ ! -L ~/.emacs ]; then
    mv ~/.emacs ~/.emacs.backup
    echo "Backed up existing .emacs"
fi

# シンボリックリンク作成
ln -sf ~/dotfiles/.emacs ~/.emacs
