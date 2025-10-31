dropdb $1

createdb $1

cat ${LEARN_SOURCE_DIR}/run-poc/atom.sql | psql $1
