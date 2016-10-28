/*
 * Copyright 2016 http://github.com/hs-web
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.hsweb.ezorm.rdb.exception;

/**
 * 验证器异常,在对表数据操作前将对数据进行验证,如果验证失败,将抛出此异常
 *
 * @author zhouhao
 * @since 1.0
 */
public class ValidationException extends RuntimeException {
    /**
     * 验证结果,捕获此异常后,可获取此对象的到验证结果
     */
    private Object validateResult;

    public ValidationException(String message, Object validateResult) {
        super(message);
        this.validateResult = validateResult;
    }

    public Object getValidateResult() {
        return validateResult;
    }
}
