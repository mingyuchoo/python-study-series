// Custom JavaScript for CrewAI Business Report Generator

document.addEventListener('DOMContentLoaded', function() {
    // Form validation enhancement
    const reportForm = document.querySelector('form[action="/generate"]');
    if (reportForm) {
        reportForm.addEventListener('submit', function(event) {
            const topicInput = document.getElementById('topic');
            if (topicInput && topicInput.value.trim() === '') {
                event.preventDefault();
                alert('보고서 주제를 입력해주세요.');
                topicInput.focus();
            } else {
                // Show loading state
                const submitButton = this.querySelector('button[type="submit"]');
                if (submitButton) {
                    submitButton.disabled = true;
                    submitButton.innerHTML = '<span class="spinner-border spinner-border-sm me-2" role="status" aria-hidden="true"></span>처리 중...';
                }
            }
        });
    }
    
    // File input enhancement
    const fileInput = document.getElementById('files');
    if (fileInput) {
        fileInput.addEventListener('change', function() {
            const fileInfo = document.createElement('div');
            fileInfo.className = 'mt-2';
            
            // Remove any existing file info
            const existingInfo = this.parentNode.querySelector('.mt-2');
            if (existingInfo) {
                existingInfo.remove();
            }
            
            if (this.files.length > 0) {
                fileInfo.innerHTML = `<strong>${this.files.length}개의 파일이 선택됨:</strong><ul class="mb-0 mt-1"></ul>`;
                const fileList = fileInfo.querySelector('ul');
                
                for (let i = 0; i < this.files.length; i++) {
                    const file = this.files[i];
                    const fileItem = document.createElement('li');
                    fileItem.textContent = `${file.name} (${formatFileSize(file.size)})`;
                    fileList.appendChild(fileItem);
                }
                
                this.parentNode.appendChild(fileInfo);
            }
        });
    }
    
    // Helper function to format file size
    function formatFileSize(bytes) {
        if (bytes === 0) return '0 Bytes';
        const k = 1024;
        const sizes = ['Bytes', 'KB', 'MB', 'GB'];
        const i = Math.floor(Math.log(bytes) / Math.log(k));
        return parseFloat((bytes / Math.pow(k, i)).toFixed(2)) + ' ' + sizes[i];
    }
    
    // Add tooltip functionality
    const tooltipTriggerList = [].slice.call(document.querySelectorAll('[data-bs-toggle="tooltip"]'));
    if (typeof bootstrap !== 'undefined') {
        tooltipTriggerList.map(function (tooltipTriggerEl) {
            return new bootstrap.Tooltip(tooltipTriggerEl);
        });
    }
});
