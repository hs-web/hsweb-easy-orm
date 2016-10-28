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
 * 触发器异常,继承自{@link RuntimeException},在执行触发器时发生错误将抛出此异常
 *
 * @author zhouhao
 * @since 1.0
 */
public class TriggerException extends RuntimeException {
    public TriggerException(String message, Throwable cause) {
        super(message, cause);
    }

    public TriggerException(String message) {
        super(message);
    }
}
