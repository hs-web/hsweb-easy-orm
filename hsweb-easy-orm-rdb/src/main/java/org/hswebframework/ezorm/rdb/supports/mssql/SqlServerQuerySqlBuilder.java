package org.hswebframework.ezorm.rdb.supports.mssql;

import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.RDBFeatureType;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.TableOrViewMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.DefaultQuerySqlBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.FragmentBlock;
import org.hswebframework.ezorm.rdb.operator.builder.Paginator;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.BlockSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.ezorm.rdb.operator.dml.query.QueryOperatorParameter;

class SqlServerQuerySqlBuilder extends DefaultQuerySqlBuilder {
    public SqlServerQuerySqlBuilder(RDBSchemaMetadata schema) {
        super(schema);
    }

    protected SqlRequest build(TableOrViewMetadata metadata, QueryOperatorParameter parameter) {
        BlockSqlFragments fragments = BlockSqlFragments.of();

        fragments.addBlock(FragmentBlock.before, SELECT);

        fragments.addBlock(FragmentBlock.selectColumn, select(parameter, metadata)
                .orElseGet(() -> PrepareSqlFragments.of().addSql("*")));

        fragments.addBlock(FragmentBlock.selectFrom, from(metadata, parameter));


        if (Boolean.TRUE.equals(parameter.getForUpdate())) {
            fragments.addBlock(FragmentBlock.selectFrom, PrepareSqlFragments.of().addSql("with(updlock)"));
        }

        join(parameter, metadata)
                .ifPresent(join -> fragments.addBlock(FragmentBlock.join, join));
        //where

        where(parameter, metadata)
                .ifPresent(where ->
                                   fragments.addBlock(FragmentBlock.where, WHERE)
                                            .addBlock(FragmentBlock.where, where));

        //group by
        groupBy(parameter, metadata)
            .ifPresent(groupBy ->
                           fragments.addBlock(FragmentBlock.groupBy, GROUP_BY)
                                    .addBlock(FragmentBlock.groupBy, groupBy));
        //having

        //order by
        orderBy(parameter, metadata)
                .ifPresent(order -> fragments.addBlock(FragmentBlock.orderBy, ORDER_BY)
                                             .addBlock(FragmentBlock.orderBy, order));

        //分页
        if (!Boolean.TRUE.equals(parameter.getForUpdate()) && parameter.getPageIndex() != null && parameter.getPageSize() != null) {
            return metadata.<Paginator>findFeature(RDBFeatureType.paginator.getId())
                           .map(paginator -> paginator.doPaging(fragments, parameter.getPageIndex(), parameter.getPageSize()))
                           .map(SqlFragments::toRequest)
                           .orElseGet(fragments::toRequest);

        }
        return fragments.toRequest();
    }
}
