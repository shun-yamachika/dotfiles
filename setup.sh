#!/bin/bash

DOTFILES_DIR=~/dotfiles
FILES=".emacs .bashrc .gitconfig .xinitrc .Xdefaults .Xmodmap .xpywmrc .screenrc .latexmkrc .x11utilrc .coloritrc .npmrc"

# 各ファイルについて処理
for file in $FILES; do
    # 既存ファイルがあり、シンボリックリンクでない場合はバックアップ
    if [ -f ~/$file ] && [ ! -L ~/$file ]; then
        mv ~/$file ~/$file.backup
        echo "Backed up existing $file"
    fi

    # シンボリックリンク作成
    if [ -f $DOTFILES_DIR/$file ]; then
        ln -sf $DOTFILES_DIR/$file ~/$file
        echo "Created symlink for $file"
    else
        echo "Warning: $file not found in $DOTFILES_DIR"
    fi
done

echo "Setup complete!"
