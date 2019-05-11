# cl-kanjichart
## Requirements
- SBCL
- QuickLisp
- Anki (only needs `Anki2/User 1/collection.anki2` file)
- `kanjidic2.xml` from the [KANJIDIC Project](https://www.edrdg.org/wiki/index.php/KANJIDIC_Project)

For installation:
- make
- [buildapp](https://www.xach.com/lisp/buildapp/)
## Usage
### Using SBCL
```common-lisp
(load "your-quicklisp-installation/setup.lisp")
(load "svg.lisp")
(in-package :PW.STIEWITZ.KANJICHART.SVG)
(pw.stiewitz.kanjichart.kanjidic:read-kanjidic2.xml "kanjidic2.xml")
(pw.stiewitz.kanjichart.anki:load-from-kanji "collection.anki2")
(make-kanji-chart "output.svg")
```
### Using buildapp
```shell
make
./kanji-chart.app kanjidic2.xml collection.anki2 output.svg
```
## Bugs
The XML Parser does not support DOCTYPE tags and `kanjidic2.xml` sadly has one at the top of the file. For now, it needs to be removed manually.
