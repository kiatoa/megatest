client: rpctest-continuous-client test.db
	rpctest-continuous-client client test.db

server: rpctest-continuous-client test.db
	rpctest-continuous-client server test.db

rpctest-continuous-client: rpctest-continuous-client.scm
	csc rpctest-continuous-client.scm

test.db:
	sqlite3 test.db "CREATE TABLE foo (id INTEGER PRIMARY KEY, var TEXT, val TEXT);"
