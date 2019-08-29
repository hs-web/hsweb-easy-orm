package org.hswebframework.ezorm.rdb.operator.dml;

import org.hswebframework.ezorm.core.Conditional;
import org.hswebframework.ezorm.core.MethodReferenceColumn;
import org.hswebframework.ezorm.core.StaticMethodReferenceColumn;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.operator.DQLOperator;

import java.util.function.Consumer;

/**
 * database
 * .dml()
 * .query()
 * .select(count("id","total"))
 * .from("user")
 * .where(dsl->dsl.is("name","1"))
 * .execute()
 * .reactive(map())
 * .subscribe(data->);
 */
public interface Query {

    Query select(String... columns);

    Query select(StaticMethodReferenceColumn... columns);

    Query select(MethodReferenceColumn... columns);

    Query select(ColumnOperator... operators);

    Query from(String name);

    Query where(Consumer<Conditional<?>> conditionalConsumer);

    Query where(ColumnOperator... condition);

    Query where(Object condition);

    Query join(String target, ColumnOperator... on);

    Query leftJoin(String target, ColumnOperator... on);

    Query rightJoin(String target, ColumnOperator... on);

    Query fullJoin(String target, ColumnOperator... on);

    Query groupBy(ColumnOperator... operators);

    Query having(ColumnOperator... operators);

    Query limit(int limit, int offset);

    Query forUpdate();

    SqlRequest getSql();

    <E,R> DQLOperator execute(ResultWrapper<E,R> wrapper);
}
