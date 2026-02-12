package org.hswebframework.ezorm.rdb.supports.kingbase.mysql;

import org.hswebframework.ezorm.rdb.codec.EnumValueCodec;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.CompositeExceptionTranslation;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlAlterTableSqlBuilder;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlEnumInFragmentBuilder;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlIndexMetadataParser;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlPaginator;
import org.hswebframework.ezorm.rdb.supports.postgres.PostgresqlR2DBCExceptionTranslation;
import org.hswebframework.ezorm.rdb.utils.FeatureUtils;

/**
 * KingbaseES MySQL 兼容模式的 Schema 元数据.
 * <p>
 * KingbaseES 底层使用 PostgreSQL 协议通信，但 SQL 语法兼容 MySQL。因此：
 * <ul>
 *   <li>DDL 构建器：使用 KingbaseES 适配版（去掉 ENGINE=/CHARSET=）</li>
 *   <li>分页器：复用 MySQL 的 LIMIT ?,? 语法</li>
 *   <li>元数据解析器：复用 MySQL information_schema 查询</li>
 *   <li>异常翻译：使用 <b>PostgreSQL</b> 异常翻译（因为驱动层是 r2dbc-postgresql）</li>
 *   <li>方言：使用 {@link KingbaseMysqlDialect}（MySQL 类型映射 + 双引号引用）</li>
 * </ul>
 *
 * @since 4.2
 */
public class KingbaseMysqlSchemaMetadata extends RDBSchemaMetadata {

    public KingbaseMysqlSchemaMetadata(String name) {
        super(name);

        // DDL 构建器 - 去掉 ENGINE=/CHARSET=
        addFeature(new KingbaseMysqlCreateTableSqlBuilder());
        addFeature(new MysqlAlterTableSqlBuilder());

        // 分页器 - 复用 MySQL 的 LIMIT ?,? 语法
        addFeature(new MysqlPaginator());

        // 元数据解析器
        addFeature(new KingbaseMysqlTableMetadataParser(this));
        addFeature(new MysqlIndexMetadataParser(this));

        // 方言
        addFeature(KingbaseMysqlDialect.global);

        // 异常翻译 - 使用 PostgreSQL（因为底层是 PG 协议驱动）
        addFeature(new CompositeExceptionTranslation()
                           .add(FeatureUtils.r2dbcIsAlive(), () -> PostgresqlR2DBCExceptionTranslation.of(this))
        );
    }

    @Override
    public RDBTableMetadata newTable(String name) {
        RDBTableMetadata metadata = super.newTable(name);
        metadata.addFeature(new KingbaseMysqlBatchUpsertOperator(metadata));
        metadata.setOnColumnAdded(column -> {
            if (column.getValueCodec() instanceof EnumValueCodec && ((EnumValueCodec) column.getValueCodec()).isToMask()) {
                column.addFeature(MysqlEnumInFragmentBuilder.in);
                column.addFeature(MysqlEnumInFragmentBuilder.notIn);
            }
        });
        return metadata;
    }

    @Override
    public void addTable(RDBTableMetadata metadata) {
        metadata.addFeature(new KingbaseMysqlBatchUpsertOperator(metadata));
        super.addTable(metadata);
    }
}
