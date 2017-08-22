# public-body-classification-data

To find matching records in the government-organisation and local-authority-*
registers:

1. Run `R/download-organisations.R` to create `lists/organisation.tsv`
1. Run `R/extract-public-bodies.R` to create `lists/public-body.tsv`
1. Use my [fork](https://github.com/nacnudus/csvdedupe) of csvdedupe to do the
   probabalistic match.
   ```sh
   csvlink lists/public-body.csv lists/organisation.csv \
   --field_names name \
   --output_file lists/auto-joined.csv \
   --training_file training.json
   ```
1. Manually check the matches in [Google Sheets](https://docs.google.com/spreadsheets/d/173kw3ohF3OAVt1JK_V-x9vbQkH96iyYXKlNXgyCrKBc/edit#gid=0).
