package org.hswebframework.ezorm.rdb.operator.builder.fragments;

import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;

import java.util.List;

public interface SqlFragments {

    boolean isEmpty();

    default boolean isNotEmpty(){
        return !isEmpty();
    }

    List<String> getSql();

    List<Object> getParameters();

   default SqlRequest toRequest(){
       return SqlRequests.prepare(String.join(" ",getSql()),getParameters().toArray());
   }
}
