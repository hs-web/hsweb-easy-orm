package org.hswebframework.ezorm.rdb.operator;

import org.hswebframework.ezorm.rdb.metadata.builder.TableBuilder;

public interface DDLOperator {

    TableBuilder createOrAlter(String name);

}
