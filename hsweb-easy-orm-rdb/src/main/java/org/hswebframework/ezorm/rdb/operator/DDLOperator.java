package org.hswebframework.ezorm.rdb.operator;

import org.hswebframework.ezorm.rdb.operator.ddl.TableBuilder;

public interface DDLOperator {

    TableBuilder createOrAlter(String name);

}
