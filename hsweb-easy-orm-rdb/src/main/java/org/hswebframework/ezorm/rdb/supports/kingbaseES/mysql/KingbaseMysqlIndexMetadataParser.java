package org.hswebframework.ezorm.rdb.supports.kingbaseES.mysql;

import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.supports.kingbaseES.KingbaseIndexMetadataParser;

/**
 * KingbaseES exposes index metadata through PostgreSQL-compatible catalog views.
 */
public class KingbaseMysqlIndexMetadataParser extends KingbaseIndexMetadataParser {

    public KingbaseMysqlIndexMetadataParser(RDBSchemaMetadata schema) {
        super(schema);
    }
}
