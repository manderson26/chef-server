-- poor man's sqitch
-- sudo su opscode-pgsql -c "/opt/opscode/embedded/bin/psql bookshelf -f /host/src/bookshelf/schema_wip.sql"

DROP INDEX file_names_file_id_index;
DROP INDEX file_data_hash_md5_index;
DROP INDEX file_data_hash_sha512_index;
DROP INDEX file_chunks_id_chunk_index;

DROP INDEX bucket_names_bucket_id_index CASCADE;

ALTER TABLE file_names DROP CONSTRAINT file_names_file_id_fk;
ALTER TABLE file_chunks DROP CONSTRAINT file_chunks_id_fk;

DROP FUNCTION IF EXISTS create_file(file_names.bucket_id%TYPE, file_names.name%TYPE);

DROP TABLE IF EXISTS file_chunks;
DROP TABLE IF EXISTS file_data;
DROP TABLE IF EXISTS file_names;
DROP TABLE IF EXISTS bucket_names;

GRANT ALL PRIVILEGES ON DATABASE bookshelf TO opscode_chef;
ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT SELECT, INSERT, UPDATE, DELETE ON TABLES TO opscode_chef
GRANT USAGE ON ALL SEQUENCES IN SCHEMA public to opscode_chef;;
-- GRANT ALL PRIVILEGES ON ALL TABLEs in SCHEMA public 
GRANT ALL PRIVILEGES ON TABLE file_names TO opscode_chef;
GRANT ALL PRIVILEGES ON TABLE file_data TO opscode_chef;
GRANT ALL PRIVILEGES ON TABLE file_chunks TO opscode_chef;

--
-- Encoding choices:
-- Hash size  bytes hex base64
-- md5  160    20    40
-- sha  256    32    64
-- sha  512    64   128
CREATE TABLE IF NOT EXISTS bucket_names(
   bucket_name text NOT NULL PRIMARY KEY,
   bucket_id serial NOT NULL
);

CREATE UNIQUE INDEX bucket_names_bucket_id_index on bucket_names(bucket_id);

-- By convention we can infer the org id from the name used. Some
-- maintenance operations would be much faster if we could index on
-- that. For now, we will want to make sure we can index on a prefix
-- efficiently.
CREATE TABLE IF NOT EXISTS file_names(
    bucket_id  int NOT NULL,
    name       text NOT NULL,
    created_at timestamp without time zoned default (now() at time zone 'utc'),
    CONSTRAINT file_names_bucket_name_key UNIQUE(bucket_id, name),
    data_id    bigint NOT NULL
);

CREATE UNIQUE INDEX file_names_file_id_index ON file_names(data_id);

-- When the bucket is deleted, we should delete all the files in it.
ALTER TABLE file_names ADD CONSTRAINT file_names_bucket_names_fk FOREIGN KEY (bucket_id) REFERENCES bucket_names(bucket_id) ON DELETE CASCADE;

--
-- This is separate from the file table because that is apparently a
-- good pattern if we use blobs with OIDs, and it also allows
-- deduplication.
--
CREATE TABLE IF NOT EXISTS file_data(
    data_id     bigserial PRIMARY KEY,
    complete	boolean,
    chunk_count int,
    -- Normal practice would be to constrain hash_* fields to be NOT
    -- NULL UNIQUE, but if we are streaming the file we won't know
    -- those until the end. We could use a dummy hash and change it
    -- after the fact. Also, there exist known collisions for md5;
    -- relying on it being unique is unwise.
    hash_md5    bytea, -- 160 bits as binary (20B)

    -- Might want to store sha256 as well/instead, because the S3 v4 api has a field for that.
    -- This exists to allow deduplication. sha512 is faster than
    -- sha256, and 32 extra bytes per file seems pretty low impact.
    -- 256 bits would be ample for simple collision by accident, but
    -- 512 offers resistance against dedicated attack.
    -- rule of thumb for dedup is prob of collision is p = number_of_files^2/(2*2^bits) or
    hash_sha512 bytea -- 512 bits as binary (64B)
);


CREATE INDEX file_data_hash_md5_index ON file_data(hash_md5);
CREATE INDEX file_data_hash_sha512_index ON file_data(hash_sha512);

-- This constraint doesn't provide reference counting for file_data;
ALTER TABLE file_names ADD CONSTRAINT file_names_file_id_fk FOREIGN KEY (data_id) REFERENCES file_data(data_id) ON DELETE RESTRICT;

-- Reference counting happens here:
CREATE OR REPLACE FUNCTION delete_file_last_reference() RETURNS TRIGGER as $delete_file_last_reference$
   BEGIN
       IF EXISTS (SELECT 1 FROM file_names WHERE data_id = OLD.data_id) THEN
           DELETE FROM file_data where data_id = OLD.data_id;
       END IF;
   END;
$delete_file_last_reference$ LANGUAGE plpgsql;

-- This trigger probably needs to be done on update of data_id
CREATE TRIGGER delete_file_last_reference AFTER DELETE ON file_names FOR EACH ROW EXECUTE PROCEDURE delete_file_last_reference();

-- Storage of data as chunks avoids 1GB limit on bytea structures, allows more efficient streaming
--
CREATE TABLE IF NOT EXISTS file_chunks(
    data_id     bigint,
    chunk       integer,
    CONSTRAINT file_chunks_data_id_chunk_key UNIQUE(data_id, chunk), 
    data        bytea
);

ALTER TABLE file_chunks ADD CONSTRAINT file_chunks_id_fk FOREIGN KEY (data_id) REFERENCES file_data(data_id) ON DELETE CASCADE ;

-- Insert file function to ease sqerl interface
CREATE OR REPLACE FUNCTION create_file(
       bucket_id file_names.bucket_id%TYPE,
       new_name   file_names.name%TYPE )
RETURNS file_data.data_id%TYPE -- what happens if exists already? TODO
AS $$
DECLARE
   new_id file_data.data_id%TYPE;
BEGIN
   INSERT INTO file_data (complete) VALUES ('false') returning data_id INTO new_id;
   INSERT INTO file_names (bucket_id, "name", data_id) VALUES (bucket_id, new_name, new_id);
   RETURN new_id;
END;
$$
LANGUAGE plpgsql VOLATILE;

