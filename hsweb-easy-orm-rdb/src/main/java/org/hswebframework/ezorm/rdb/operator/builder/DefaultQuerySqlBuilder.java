package org.hswebframework.ezorm.rdb.operator.builder;

import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.meta.DefaultRDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.meta.RDBFutures;
import org.hswebframework.ezorm.rdb.meta.TableOrViewMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.ComplexQueryParameter;

public class DefaultQuerySqlBuilder implements QuerySqlBuilder {

    private ComplexQueryParameter parameter;

    private DefaultRDBSchemaMetadata schema;

    @Override
    public SqlRequest build() {
        String from = parameter.getFrom();
        if (from == null || from.isEmpty()) {
            throw new UnsupportedOperationException("from table or view not set");
        }
        TableOrViewMetadata viewMetadata = schema.getTableOrView(from)
                .orElseThrow(() -> new UnsupportedOperationException("table or view [" + from + "] doesn't exist "));

        //where 片段构造器
        SqlFragmentBuilder where = RDBFutures.where(viewMetadata);


        return null;
    }
}
