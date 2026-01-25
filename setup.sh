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

# lib/emacs ディレクトリの処理
if [ -d $DOTFILES_DIR/lib/emacs ]; then
    # lib ディレクトリが存在しない場合は作成
    mkdir -p ~/lib

    # config-org.el のシンボリックリンク作成
    if [ -f $DOTFILES_DIR/lib/emacs/config-org.el ]; then
        if [ -f ~/lib/emacs/config-org.el ] && [ ! -L ~/lib/emacs/config-org.el ]; then
            mv ~/lib/emacs/config-org.el ~/lib/emacs/config-org.el.backup
            echo "Backed up existing config-org.el"
        fi
        mkdir -p ~/lib/emacs
        ln -sf $DOTFILES_DIR/lib/emacs/config-org.el ~/lib/emacs/config-org.el
        echo "Created symlink for lib/emacs/config-org.el"
    fi

    # config-org-latex.el のシンボリックリンク作成
    if [ -f $DOTFILES_DIR/lib/emacs/config-org-latex.el ]; then
        if [ -f ~/lib/emacs/config-org-latex.el ] && [ ! -L ~/lib/emacs/config-org-latex.el ]; then
            mv ~/lib/emacs/config-org-latex.el ~/lib/emacs/config-org-latex.el.backup
            echo "Backed up existing config-org-latex.el"
        fi
        mkdir -p ~/lib/emacs
        ln -sf $DOTFILES_DIR/lib/emacs/config-org-latex.el ~/lib/emacs/config-org-latex.el
        echo "Created symlink for lib/emacs/config-org-latex.el"
    fi
fi

# IEEEtran.cls のセットアップ
if [ -f $DOTFILES_DIR/IEEEtran.cls ]; then
    # ユーザーローカルのtexmfディレクトリを作成
    mkdir -p ~/texmf/tex/latex/ieeetran

    # IEEEtran.clsをコピー
    cp $DOTFILES_DIR/IEEEtran.cls ~/texmf/tex/latex/ieeetran/
    echo "Copied IEEEtran.cls to ~/texmf/tex/latex/ieeetran/"

    # texhashを実行してファイルデータベースを更新（可能な場合）
    if command -v mktexlsr &> /dev/null; then
        mktexlsr ~/texmf
        echo "Updated TeX file database"
    fi
fi

echo "Setup complete!"
