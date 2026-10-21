package org.hswebframework.ezorm.rdb.supports.kingbase.mysql;

import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.supports.kingbase.KingbaseIndexMetadataParser;

/**
 * KingbaseES exposes index metadata through PostgreSQL-compatible catalog views.
 */
public class KingbaseMysqlIndexMetadataParser extends KingbaseIndexMetadataParser {

    public KingbaseMysqlIndexMetadataParser(RDBSchemaMetadata schema) {
        super(schema);
    }
}
