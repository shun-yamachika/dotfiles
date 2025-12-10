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

# .config ディレクトリの処理
if [ -d $DOTFILES_DIR/.config/fish ]; then
    # .config ディレクトリが存在しない場合は作成
    mkdir -p ~/.config

    # 既存のfishディレクトリがあり、シンボリックリンクでない場合はバックアップ
    if [ -d ~/.config/fish ] && [ ! -L ~/.config/fish ]; then
        mv ~/.config/fish ~/.config/fish.backup
        echo "Backed up existing .config/fish"
    fi

    # シンボリックリンク作成
    ln -sf $DOTFILES_DIR/.config/fish ~/.config/fish
    echo "Created symlink for .config/fish"
fi

echo "Setup complete!"
