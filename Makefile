entry = pw.stiewitz.kanjichart.svg:main

all:
	buildapp --load ~/quicklisp/setup.lisp --load muffle.lisp --load svg.lisp --entry ${entry} --output kanji-chart.app
