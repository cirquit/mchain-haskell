CC = ghc
DBOUT  = -o bin/mchain-db
MAPOUT = -o bin/mchain-map
JOUT   = -o bin/jchan
CFLAGS = -O2

mc-map:   map      mapclean
mc-db:    database dbclean
jchan: 		jsonchan jclean

map:

	$(CC) $(CFLAGS) $(MAPOUT) \
		src/mc-map/MarkovMap.hs

database:

	$(CC) $(CFLAGS) $(DBOUT) \
		src/mc-db/MarkovDB.hs\
		src/mc-db/Prefix.hs


jsonchan:

	$(CC) $(CFLAGS) $(JOUT) \
		src/json/ChanTypes.hs\
		src/json/4json.hs


mapclean:
	rm -f src/mc-map/*.dyn_o
	rm -f src/mc-map/*.dyn_hi
	rm -f src/mc-map/*.o
	rm -f src/mc-map/*.hi

dbclean:
	rm -f src/mc-db/*.dyn_o
	rm -f src/mc-db/*.dyn_hi
	rm -f src/mc-db/*.o
	rm -f src/mc-db/*.hi

jclean:
	rm -f src/json/*.o
	rm -f src/json/*.hi
