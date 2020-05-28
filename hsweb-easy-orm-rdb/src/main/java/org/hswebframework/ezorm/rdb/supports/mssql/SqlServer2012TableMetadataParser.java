package org.hswebframework.ezorm.rdb.supports.mssql;

import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.supports.commons.RDBTableMetadataParser;

/**
 * @author zhouhao
 */
public class SqlServer2012TableMetadataParser extends RDBTableMetadataParser {

    public SqlServer2012TableMetadataParser(RDBSchemaMetadata schema) {
        super(schema);
    }

    private static final String TABLE_META_SQL = String.join(" ",
            "SELECT ",
            "c.name as name,",
            "t.name as data_type,",
            "c.length as data_length,",
            "c.xscale as data_scale,",
            "c.xprec as data_precision,",
            "case when c.isnullable=1 then 0 else  1 end as [not_null],",
            "cast(p.value as varchar(500)) as comment ",
            "FROM syscolumns c ",
            "inner join  systypes t on c.xusertype = t.xusertype ",
            "left join sys.extended_properties p on c.id=p.major_id and c.colid=p.minor_id ",
            "WHERE c.id = object_id(#{table})");


    @Override
    protected String getTableMetaSql(String tname) {
        return TABLE_META_SQL;
    }

    @Override
    protected String getTableCommentSql(String tname) {
        return "select cast(p.value as varchar(500)) as comment " +
                "from sys.extended_properties p " +
                " where p.major_id=object_id(#{table}) and p.minor_id=0";
    }

    @Override
    protected String getAllTableSql() {
        return "select name from sysobjects where xtype='U'";
    }

    @Override
    protected String getTableExistsSql() {
        return "select count(1) as total from sysobjects where xtype='U' and name = #{table}";
    }
}
