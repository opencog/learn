dropdb $1

createdb $1

cat ~/MyOpenCogSources/atomspace/opencog/persist/sql/multi-driver/atom.sql | psql $1
