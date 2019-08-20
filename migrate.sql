/* Migrations for priorganize.el */

CREATE TABLE IF NOT EXISTS queues
( id VARCHAR(36) PRIMARY KEY,
  name VARCHAR(255) UNIQUE NOT NULL,
  description TEXT
);

INSERT OR IGNORE INTO queues
VALUES (
  'd3f8e89c-8994-5d24-ff33-5610abfaf294',
  'global',
  'All items enter this queue for a global prioritization.'
);

CREATE TABLE IF NOT EXISTS items
( id VARCHAR(36) PRIMARY KEY,
  name VARCHAR(255) UNIQUE NOT NULL,
  description TEXT
);

/* Not only relate items to queue, but set the stage for
the priority queue impl */
CREATE TABLE IF NOT EXISTS items_queues
( item_id VARCHAR(36) NOT NULL REFERENCES items(id),
  queue_id VARCHAR(36) NOT NULL REFERENCES queue(id),
  parent VARCHAR(36) REFERENCES items(id)
);
