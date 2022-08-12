import pymysql.cursors
from random import randint
from time import sleep

# Connect to the database
connection = pymysql.connect(host='localhost',
                             user='root',
                             password='root-password-here',
                             db='test',
                             charset='utf8mb4',
                             cursorclass=pymysql.cursors.DictCursor)

try:
    while True:
        rnd = randint(-50, 100)

        with connection.cursor() as cursor:
            # Create a new record
            sql = "INSERT INTO `temp` (`temper`) VALUES (%s)"
            cursor.execute(sql, (rnd))

        # connection is not autocommit by default. So you must commit to save
        # your changes.
        connection.commit()

        with connection.cursor() as cursor:
            # Read a single record
            sql = "SELECT count(*) FROM `temp`"
            cursor.execute(sql)
            result = cursor.fetchone()
            print(result)
        sleep(1)

finally:
    connection.close()
