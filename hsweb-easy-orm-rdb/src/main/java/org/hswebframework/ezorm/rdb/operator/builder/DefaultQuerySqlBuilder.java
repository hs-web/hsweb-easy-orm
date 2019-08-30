package org.hswebframework.ezorm.rdb.operator.builder;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.meta.DefaultRDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.meta.RDBFutures;
import org.hswebframework.ezorm.rdb.meta.TableOrViewMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.QuerySqlFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.ComplexQueryParameter;

@AllArgsConstructor
public class DefaultQuerySqlBuilder implements QuerySqlBuilder {

    private ComplexQueryParameter parameter;

    private DefaultRDBSchemaMetadata schema;

    @Override
    public SqlRequest build() {
        String from = parameter.getFrom();
        if (from == null || from.isEmpty()) {
            throw new UnsupportedOperationException("from table or view not set");
        }
        TableOrViewMetadata metadata = schema.getTableOrView(from)
                .orElseThrow(() -> new UnsupportedOperationException("table or view [" + from + "] doesn't exist "));
        PrepareSqlFragments fragments = PrepareSqlFragments.of();

        fragments.addSql("select");

        fragments.addFragments(RDBFutures.select(metadata).createFragments(parameter));

        fragments.addSql("from")
                .addSql(from);

        // TODO: 2019-08-30 JOIN

        fragments.addSql("where");

        //where 片段构造器
        QuerySqlFragmentBuilder where = RDBFutures.where(metadata);
        fragments.addFragments(where.createFragments(parameter));


        return fragments.toRequest();
    }
}
