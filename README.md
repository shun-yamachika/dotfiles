# dotfiles

個人的な設定ファイルを管理するリポジトリ

## セットアップ手順

### 1. fishのインストール

```bash
sudo apt-get update
sudo apt-get install -y fish
```

### 2. emacsのインストール

```bash
sudo apt-get install -y emacs
```

### 3. screenのインストール

```bash
sudo apt-get install -y screen
```

### 4. xpywm（ウィンドウマネージャ）のダウンロード

```bash
wget -O - lsnl.jp/xpywm | sh
```

### 5. home.tgzの展開

```bash
# home.tgzをホームディレクトリに配置後
cd ~
tar xzvf home.tgz
```

### 6. dotfilesのセットアップ

```bash
git clone <repository-url> ~/dotfiles
cd ~/dotfiles
./setup.sh
```

## 含まれる設定ファイル

- `.bashrc` - Bash設定
- `.emacs` - Emacs設定
- `.config/fish/` - Fish shell設定
- `.gitconfig` - Git設定
- `.xinitrc`, `.Xdefaults`, `.Xmodmap` - X Window System設定
- `.xpywmrc` - xpywmウィンドウマネージャ設定
- `.screenrc` - GNU Screen設定
- `.latexmkrc` - LaTeX設定
- その他各種設定ファイル
