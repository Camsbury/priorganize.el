/* Migrations for priorganize.el */

CREATE TABLE IF NOT EXISTS queues
( id VARCHAR(36) PRIMARY KEY,
  name VARCHAR(255) NOT NULL,
  description TEXT
);

INSERT INTO queues
VALUES (
  'd3f8e89c-8994-5d24-ff33-5610abfaf294',
  'global',
  'All items enter this queue for a global prioritization.'
);
