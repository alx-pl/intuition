#!/usr/bin/python

import os
import subprocess
import sqlite3

def create_connection(db_file):
    """ create a database connection to the SQLite database
        specified by db_file
    :param db_file: database file
    :return: Connection object or None
    """
    conn = None
    try:
        conn = sqlite3.connect(db_file, cached_statements=10000)
    except Error as e:
        print(e)

    return conn



dbconn=create_connection('formulas.db')
if dbconn is None:
    print('Problem z plikiem bazy')
    exit(1)
cursor=dbconn.cursor()

res = cursor.execute("SELECT count(id) FROM formulas WHERE status like '[Just (MError \"Tim%';")
print("Vanilla run out of the time " + str(res.fetchone()[0]))
res = cursor.execute("SELECT count(id) FROM formulas WHERE statusq1 like '[Just (MError \"Tim%';")
print("Optimised run out of the time " + str(res.fetchone()[0]))
res = cursor.execute("SELECT count(id) FROM formulas WHERE status like '[Just (MError \"Tim%' and statusq1 like '[Just (MError \"Tim%';")
print("Both run out of the time " + str(res.fetchone()[0]))


res = cursor.execute("SELECT count(id) FROM formulas WHERE rtimev < rtimevq1 and rtimevq1 < 4.9;")
print("Vanilla better than optimised in " + str(res.fetchone()[0]))
res = cursor.execute("SELECT count(id) FROM formulas WHERE rtimev > rtimevq1 and rtimev < 4.9;")
print("Vanilla worse than optimised in " + str(res.fetchone()[0]))


s1template="SELECT count(id) FROM formulas WHERE __LOWER__ <= __TVAL__ AND __TVAL__ < __UPPER__;"

step=0.00010
lower=0.0
upper=lower + step
top=0.00110
print("Time results for vanila prover")
while lower < top:
  
  sel = s1template.replace("__TVAL__", "rtimev")
  sel = sel.replace("__LOWER__", str(lower))
  sel = sel.replace("__UPPER__", str(upper))
  res = cursor.execute(sel)
  print("[" + str(lower) + ", " + str(upper) + ")  -- " + str(res.fetchone()[0]))
  lower = upper
  upper = upper + step

sel = s1template.replace("__TVAL__", "rtimev")
sel = sel.replace("__LOWER__", str(top))
sel = sel.replace("__UPPER__", "4.8")
res = cursor.execute(sel)
print(res.fetchone()[0])

print("Time results for optimised prover")
lower=0.0
upper=lower + step
print(s1template)
while lower < top:
  
  sel = s1template.replace("__TVAL__", "rtimevq1")
  sel = sel.replace("__LOWER__", str(lower))
  sel = sel.replace("__UPPER__", str(upper))
  res = cursor.execute(sel)
  print("[" + str(lower) + ", " + str(upper) + ")  -- " + str(res.fetchone()[0]))
  lower = upper
  upper = upper + step

sel = s1template.replace("__TVAL__", "rtimevq1")
sel = sel.replace("__LOWER__", str(top))
sel = sel.replace("__UPPER__", "4.8")
res = cursor.execute(sel)
print(res.fetchone()[0])


s1template="SELECT count(id) FROM formulas WHERE __LOWER__ <= __TVAL__ AND __TVAL__ < __UPPER__ and rtimevq1 > rtimev;"
print("Cases when optimised prover performed worse then naive one")
lower=0.0
upper=lower + step
while lower < top:
  
  sel = s1template.replace("__TVAL__", "rtimev")
  sel = sel.replace("__LOWER__", str(lower))
  sel = sel.replace("__UPPER__", str(upper))
  res = cursor.execute(sel)
  print("[" + str(lower) + ", " + str(upper) + ")  -- " + str(res.fetchone()[0]))
  lower = upper
  upper = upper + step

sel = s1template.replace("__TVAL__", "rtimevq1")
sel = sel.replace("__LOWER__", str(top))
sel = sel.replace("__UPPER__", "4.8")
res = cursor.execute(sel)
print(res.fetchone()[0])


s1template="SELECT count(id) FROM formulas WHERE __LOWER__ <= __TVAL__ AND __TVAL__ < __UPPER__ and rtimevq1 < rtimev;"
print("Cases when optimised prover performed better then naive one")
lower=0.0
upper=lower + step
while lower < top:
  
  sel = s1template.replace("__TVAL__", "rtimev")
  sel = sel.replace("__LOWER__", str(lower))
  sel = sel.replace("__UPPER__", str(upper))
  res = cursor.execute(sel)
  print("[" + str(lower) + ", " + str(upper) + ")  -- " + str(res.fetchone()[0]))
  lower = upper
  upper = upper + step

sel = s1template.replace("__TVAL__", "rtimevq1")
sel = sel.replace("__LOWER__", str(top))
sel = sel.replace("__UPPER__", "4.8")
res = cursor.execute(sel)
print(res.fetchone()[0])

cursor.close()
dbconn.close()



