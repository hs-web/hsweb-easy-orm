package org.hswebframework.ezorm.rdb.supports.kingbase.mysql;

import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlTableMetadataParser;

/**
 * KingbaseES MySQL 兼容模式的表元数据解析器.
 * <p>
 * KingbaseES MySQL 兼容模式的 {@code information_schema.columns} 和
 * {@code information_schema.tables} 与 MySQL 高度兼容，
 * 因此直接继承 {@link MysqlTableMetadataParser}。
 * <p>
 * 如果后续发现字段差异（如 {@code column_type} 不存在等），
 * 可在此类中覆盖对应的 SQL 方法。
 *
 * @since 4.2
 */
public class KingbaseMysqlTableMetadataParser extends MysqlTableMetadataParser {

    public KingbaseMysqlTableMetadataParser(RDBSchemaMetadata schema) {
        super(schema);
    }

}
