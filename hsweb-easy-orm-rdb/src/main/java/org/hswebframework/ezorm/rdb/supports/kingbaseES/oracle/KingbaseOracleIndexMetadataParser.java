package org.hswebframework.ezorm.rdb.supports.kingbaseES.oracle;

import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.supports.kingbaseES.KingbaseIndexMetadataParser;

import java.util.Locale;

/**
 * KingbaseES Oracle-mode index metadata parser.
 */
public class KingbaseOracleIndexMetadataParser extends KingbaseIndexMetadataParser {

    private static final String INDEX_SQL = String.join(" ",
        "select i.relname as index_name, t.relname as table_name, ix.indisunique as uniqueness,",
        "case when k.attnum = 0 then pg_get_indexdef(ix.indexrelid, cast(k.pos as integer), true)",
        "else a.attname end as column_name, k.pos as column_position,",
        "case when (ix.indoption[k.pos-1] & 1) = 1 then 'DESC' else 'ASC' end as descend",
        "from pg_class t join pg_index ix on t.oid = ix.indrelid",
        "join pg_class i on i.oid = ix.indexrelid",
        "join pg_namespace n on n.oid = t.relnamespace",
        "cross join lateral unnest(ix.indkey) with ordinality as k(attnum, pos)",
        "left join pg_attribute a on a.attrelid = t.oid and a.attnum = k.attnum",
        "where upper(n.nspname) = ?");

    private static final String PRIMARY_KEY_SQL = String.join(" ",
        "select i.relname as index_name from pg_index ix",
        "join pg_class t on t.oid = ix.indrelid",
        "join pg_class i on i.oid = ix.indexrelid",
        "join pg_namespace n on n.oid = t.relnamespace",
        "where ix.indisprimary and upper(n.nspname) = ?");

    public KingbaseOracleIndexMetadataParser(RDBSchemaMetadata schema) {
        super(schema);
    }

    public static KingbaseOracleIndexMetadataParser of(RDBSchemaMetadata schema) {
        return new KingbaseOracleIndexMetadataParser(schema);
    }

    @Override
    protected String indexSql(boolean byName) {
        return INDEX_SQL + (byName ? " and upper(i.relname) = ?" : " and upper(t.relname) like ?");
    }

    @Override
    protected String primaryKeySql(boolean byName) {
        return PRIMARY_KEY_SQL + (byName ? " and upper(i.relname) = ?" : " and upper(t.relname) like ?");
    }

    @Override
    protected SqlRequest request(String sql, String name) {
        // Match native Oracle name normalization while retaining Kingbase physical identifiers.
        return SqlRequests.prepare(sql, getSchema().getName().toUpperCase(Locale.ROOT), name.toUpperCase(Locale.ROOT));
    }
}
