all: doc

doc:
	pasdoc core/sqlitei.pas  -E ./help  -T SqliteI --marker=: --use-tipue-search --write-uses-list