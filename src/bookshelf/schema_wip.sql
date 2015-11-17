

--
-- Encoding choices:
-- Hash size  bytes hex base64
-- md5  160    20    40 
-- sha  256    32    64
-- sha  512    64   128

-- By convention we can infer the org id from the name used. Some
-- maintenance operations would be much faster if we could index on
-- that. For now, we will want to make sure we can index on a prefix
-- efficiently.
CREATE TABLE IF NOT EXISTS file_names(
    bucket  text,
    name    text,
    CONSTRAINT file_names_bucket_name_key UNIQUE(bucket, name),
    data_id bigserial
);

CREATE UNIQUE INDEX file_names_file_id_index ON file_names(data_id);

-- This constraint doesn't provide reference counting for file_data;
-- ALTER TABLE file_names ADD CONSTRAINT file_names_file_id_fk FOREIGN KEY file_id REFERENCES file_data(id) ON DELETE RESTRICT;

-- 
-- This is separate from the file table because that is apparently a
-- good pattern if we use blobs with OIDs, and it also allows
-- deduplication.
-- 
CREATE TABLE IF NOT EXISTS file_data(
    data_id          bigserial PRIMARY KEY,

    -- Normal practice would be to constrain hash_* fields to be NOT
    -- NULL UNIQUE, but if we are streaming the file we won't know
    -- those until the end. We could use a dummy hash and change it
    -- after the fact. Also, there exist known collisions for md5;
    -- relying on it being unique is unwise.
    hash_md5    bytea, -- 160 bits as binary (20B)

    -- This exists to allow deduplication. sha512 is faster than
    -- sha256, and 32 extra bytes per file seems pretty low impact.
    hash_sha512 bytea, -- 512 bits as binary (64B)

    chunk_count int
);


CREATE INDEX file_data_hash_md5_index ON file_data(hash_md5);
CREATE INDEX file_data_hash_sha512_index ON file_data(hash_sha512);

-- Reference counting happens here:
CREATE OR REPLACE FUNCTION delete_file_last_reference() RETURNS TRIGGER as $delete_file_last_reference$
   BEGIN 
       IF EXISTS (SELECT 1 FROM file_names WHERE data_id = OLD.data_id) THEN
           DELETE FROM file_data where data_id = OLD.data_id;
       END IF;
   END;
$delete_file_last_reference$ LANGUAGE plpgsql;

CREATE TRIGGER delete_file_last_reference AFTER DELETE ON file_names FOR EACH ROW EXECUTE PROCEDURE delete_file_last_reference();

-- Storage of data as chunks avoids 1GB limit on bytea structures, allows more efficient streaming
-- 
CREATE TABLE IF NOT EXISTS file_chunks(
    id          bigserial,
    chunk       integer,
    data        bytea
);

ALTER TABLE file_chunks ADD CONSTRAINT file_chunks_id_fk FOREIGN KEY id REFERENCES file_data(id) ON DELETE CASCADE ;

CREATE UNIQUE INDEX file_chunks_id_chunk_index ON file_chunks(id,chunk);


