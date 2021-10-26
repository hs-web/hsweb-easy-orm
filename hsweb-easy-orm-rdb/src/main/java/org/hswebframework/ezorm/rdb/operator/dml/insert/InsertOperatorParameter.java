package org.hswebframework.ezorm.rdb.operator.dml.insert;

import lombok.Getter;
import lombok.Setter;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

@Getter
@Setter
public class InsertOperatorParameter {

    private Set<InsertColumn> columns = new LinkedHashSet<>(64);

    private List<List<Object>> values = new ArrayList<>(32);

}
