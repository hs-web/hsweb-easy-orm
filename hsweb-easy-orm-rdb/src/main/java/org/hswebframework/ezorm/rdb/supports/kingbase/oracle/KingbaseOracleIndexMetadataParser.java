package org.hswebframework.ezorm.rdb.supports.kingbase.oracle;

import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.supports.kingbase.KingbaseIndexMetadataParser;

/**
 * KingbaseES Oracle-mode index metadata parser.
 */
public class KingbaseOracleIndexMetadataParser extends KingbaseIndexMetadataParser {

    public KingbaseOracleIndexMetadataParser(RDBSchemaMetadata schema) {
        super(schema);
    }

    public static KingbaseOracleIndexMetadataParser of(RDBSchemaMetadata schema) {
        return new KingbaseOracleIndexMetadataParser(schema);
    }
}
