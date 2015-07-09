import csv

def get_number_of_columns(csvFile):
  with open(csvFile, 'rb') as f:
    r = csv.reader(f)
    row = r.next()
  return(len(row))
