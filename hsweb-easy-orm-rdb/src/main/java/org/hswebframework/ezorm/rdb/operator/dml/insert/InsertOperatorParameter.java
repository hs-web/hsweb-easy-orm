package org.hswebframework.ezorm.rdb.operator.dml.insert;

import lombok.Getter;
import lombok.Setter;

import java.util.ArrayList;
import java.util.List;

@Getter
@Setter
public class InsertOperatorParameter {

    private String into;

    private List<InsertColumn> columns = new ArrayList<>();

    private List<List<Object>> values = new ArrayList<>();

}
