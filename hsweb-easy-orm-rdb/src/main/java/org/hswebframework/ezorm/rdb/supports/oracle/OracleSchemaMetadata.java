package org.hswebframework.ezorm.rdb.supports.oracle;

import org.hswebframework.ezorm.rdb.codec.EnumValueCodec;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;

public class OracleSchemaMetadata extends RDBSchemaMetadata {

    public OracleSchemaMetadata(String name) {
        super(name);
        addFeature(new OraclePaginator());

        addFeature(new OracleTableMetadataParser(this));
        addFeature(new OracleAlterTableSqlBuilder());

        addFeature(Dialect.ORACLE);
    }

    @Override
    public void addTable(RDBTableMetadata metadata) {
        metadata.addFeature(OracleInsertSqlBuilder.of(metadata));
        super.addTable(metadata);
    }

    @Override
    public RDBTableMetadata newTable(String name) {
        RDBTableMetadata metadata = super.newTable(name);
        metadata.addFeature(OracleInsertSqlBuilder.of(metadata));
        metadata.addFeature(new OracleBatchUpsertOperator(metadata));
        metadata.setOnColumnAdded(column->{
            if(column.getValueCodec() instanceof EnumValueCodec &&((EnumValueCodec) column.getValueCodec()).isToMask()){
                column.addFeature(OracleEnumInFragmentBuilder.in);
                column.addFeature(OracleEnumInFragmentBuilder.notIn);
            }
        });
        return metadata;
    }
}
