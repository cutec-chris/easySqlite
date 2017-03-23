all: doc

doc:
	pasdoc core/easysqlite.pas core/easysqliteorm.pas -E ./help  -T easySqlite --use-tipue-search --write-uses-list --introduction=documents/intro.txt --conclusion=documents/howto.txt
