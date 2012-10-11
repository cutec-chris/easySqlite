all: doc

doc:
	pasdoc core/sqlitei.pas core/sqliteorm.pas -E ./help  -T SqliteI --marker=: --use-tipue-search --write-uses-list