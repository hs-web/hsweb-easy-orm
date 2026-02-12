package org.hswebframework.ezorm.rdb.supports.kingbase.mysql;

import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlBatchUpsertOperator;

/**
 * KingbaseES MySQL 兼容模式的批量 Upsert 操作器.
 * <p>
 * KingbaseES MySQL 兼容版完全支持 {@code ON DUPLICATE KEY UPDATE} 语法，
 * 因此直接继承 {@link MysqlBatchUpsertOperator}，不做任何修改。
 * <p>
 * 如果后续发现语法差异，可在此类中覆盖对应方法。
 *
 * @since 4.2
 */
@SuppressWarnings("all")
public class KingbaseMysqlBatchUpsertOperator extends MysqlBatchUpsertOperator {

    public KingbaseMysqlBatchUpsertOperator(RDBTableMetadata table) {
        super(table);
    }

}
