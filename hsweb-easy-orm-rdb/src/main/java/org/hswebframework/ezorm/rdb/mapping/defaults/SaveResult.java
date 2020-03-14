package org.hswebframework.ezorm.rdb.mapping.defaults;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

/**
 * upsert保存结果.
 * <p>
 * 注意: added和updated的值并不一定准确,因为有的数据库执行upsert,无法准确获取新增和修改的结果.
 *
 * @author zhouhao
 * @version 4.0
 */
@Getter
@Setter
@AllArgsConstructor(staticName = "of")
public class SaveResult {
    private int added;

    private int updated;

    public int getTotal() {
        return added + updated;
    }

    public SaveResult merge(SaveResult result) {
        SaveResult res = SaveResult.of(added, updated);

        res.added += result.getAdded();
        res.updated += result.getUpdated();

        return res;
    }

    @Override
    public String toString() {
        return "added " + added + ",updated " + updated + ",total " + getTotal();
    }
}
