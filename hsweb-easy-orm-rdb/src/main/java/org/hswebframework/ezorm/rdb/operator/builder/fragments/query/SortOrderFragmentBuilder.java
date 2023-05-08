package org.hswebframework.ezorm.rdb.operator.builder.fragments.query;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.TableOrViewMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.EmptySqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.function.FunctionFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.query.QueryOperatorParameter;
import org.hswebframework.ezorm.rdb.operator.dml.query.SortOrder;
import reactor.function.Function3;

import static java.util.Optional.*;

@AllArgsConstructor(staticName = "of")
public class SortOrderFragmentBuilder implements QuerySqlFragmentBuilder {

    private TableOrViewMetadata metadata;

    @Override
    public SqlFragments createFragments(QueryOperatorParameter parameter) {
        PrepareSqlFragments fragments = PrepareSqlFragments.of();

        int index = 0, valueIndex = 0;
        PrepareSqlFragments orderByValue = null;

        for (SortOrder sortOrder : parameter.getOrderBy()) {
            Object value = sortOrder.getValue();
            if (null != value) {
                valueIndex++;
                if (orderByValue == null) {
                    orderByValue = PrepareSqlFragments.of();
                    orderByValue.addSql("case");
                }
                orderByValue.addSql("when")
                            .addFragments(createOrder(
                                    sortOrder,
                                    parameter,
                                    (name, column, order) -> PrepareSqlFragments
                                            .of()
                                            .addSql(name, "= ?")
                                            .addParameter(column.encode(order.getValue()))));

                orderByValue.addSql("then", String.valueOf(sortOrder.getOrder() == SortOrder.Order.desc ? 10000 + valueIndex : valueIndex));
                continue;
            }

            SqlFragments orderFragments = createOrder(sortOrder, parameter, this::createOrder);
            if (orderFragments.isNotEmpty()) {
                if (index++ != 0) {
                    fragments.addSql(",");
                }
                fragments.addFragments(orderFragments);
            }
        }

        if (null != orderByValue) {
            orderByValue.addSql("else 10000 end");

            if (fragments.isNotEmpty()) {
                orderByValue.addSql(",");
                orderByValue.addFragments(fragments);
            }
            return orderByValue;
        }

        return fragments;
    }

    private SqlFragments createOrder(String fullName, RDBColumnMetadata column, SortOrder order) {


        SqlFragments fragments = ofNullable(order.getFunction())
                .flatMap(function -> column.findFeature(FunctionFragmentBuilder.createFeatureId(function)))
                .map(builder -> builder.create(fullName, column, order))
                .orElseGet(() -> PrepareSqlFragments.of().addSql(fullName));

        return PrepareSqlFragments
                .of()
                .addFragments(fragments)
                .addSql(order.getOrder().name());
    }


    private SqlFragments createOrder(SortOrder order,
                                     QueryOperatorParameter parameter,
                                     Function3<String, RDBColumnMetadata, SortOrder, SqlFragments> builder) {
        String column = order.getColumn();
        if (column.contains(".")) {
            String[] arr = column.split("[.]");
            if (arr[0].equals(parameter.getFrom()) || arr[0].equals(parameter.getFromAlias())) {
                column = arr[1];
            } else {
                //关联表
                return parameter
                        .findJoin(arr[0])
                        .flatMap(join -> metadata
                                .getSchema()
                                .getTableOrView(join.getTarget())
                                .flatMap(table -> table.getColumn(arr[1]))
                                .map(joinColumn -> builder.apply(joinColumn.getFullName(join.getAlias()), joinColumn, order))
                        )
                        .orElse(EmptySqlFragments.INSTANCE);
            }
        }

        return metadata.getColumn(column)
                       .map(orderColumn -> builder.apply(orderColumn.getFullName(parameter.getFromAlias()), orderColumn, order))
                       .orElse(EmptySqlFragments.INSTANCE);
    }


    @Override
    public String getId() {
        return sortOrder;
    }

    @Override
    public String getName() {
        return "排序";
    }
}
